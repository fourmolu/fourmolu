{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad
import Data.Bool (bool)
import Data.List (intercalate, isSuffixOf, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text.IO.Utf8 qualified as T.Utf8
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName (PackageName)
import Language.Haskell.TH.Env (envQ)
import Options.Applicative
import Ormolu
import Ormolu.Config
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Fixity
import Ormolu.Parser (manualExts)
import Ormolu.Terminal
import Ormolu.Utils (showOutputable)
import Ormolu.Utils.Fixity
import Paths_fourmolu (version)
import System.Console.Terminal.Size qualified as Terminal
import System.Directory
import System.Exit (ExitCode (..), exitWith)
import System.FilePath qualified as FP
import System.IO (hPutStrLn, stderr)

-- | Entry point of the program.
main :: IO ()
main = do
  opts@Opts {..} <- runParser optsParserInfo
  cfg <- resolveConfig opts

  let formatOne' =
        formatOne
          optConfigFileOpts
          optMode
          optSourceType
          cfg

  exitCode <- case optInputFiles of
    [] -> formatOne' Nothing
    ["-"] -> formatOne' Nothing
    inputs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      files <- Set.toAscList . Set.fromList . concat <$> mapM getHaskellFiles inputs
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne' . Just) files
      return $
        if null errorCodes
          then ExitSuccess
          else
            ExitFailure $
              if all (== 100) errorCodes
                then 100
                else 102

  exitWith exitCode
  where
    runParser p = do
      termWidth <- maybe 100 Terminal.width <$> Terminal.size
      customExecParser (prefs $ helpIndent 35 <> columns termWidth) p

-- | Build the full config, by adding 'PrinterOpts' from a file, if found.
resolveConfig :: Opts -> IO (Config RegionIndices)
resolveConfig opts@(Opts {optConfig = cliConfig, optPrinterOpts = cliPrinterOpts}) = do
  fourmoluConfig <-
    findConfigFile' >>= \case
      Left ConfigNotFound {searchDirs} -> do
        outputDebug . unlines $
          ("No " ++ show configFileName ++ " found in any of:")
            : map ("  " ++) searchDirs
        pure emptyConfig
      Right configPath ->
        Yaml.decodeFileEither configPath >>= \case
          Left e -> do
            outputError . unlines $
              [ "Failed to load " <> configPath <> ":",
                Yaml.prettyPrintParseException e
              ]
            exitWith $ ExitFailure 400
          Right cfg -> do
            outputInfo $ "Loaded config from: " <> configPath
            outputDebug $ unwords ["*** CONFIG FILE ***", show cfg]
            pure cfg

  return $
    cliConfig
      { cfgPrinterOpts =
          resolvePrinterOpts
            [ cfgFilePrinterOpts fourmoluConfig,
              cliPrinterOpts
            ],
        cfgFixityOverrides =
          FixityOverrides . mconcat . map unFixityOverrides $
            [ cfgFixityOverrides cliConfig,
              cfgFileFixities fourmoluConfig
            ],
        cfgModuleReexports =
          ModuleReexports . mconcat . map unModuleReexports $
            [ cfgModuleReexports cliConfig,
              cfgFileReexports fourmoluConfig
            ]
      }
  where
    findConfigFile' = do
      cwd <- getCurrentDirectory
      case optInputFiles opts of
        _ | Just configPath <- optConfigFilePath opts -> pure $ Right configPath
        [] -> findConfigFile cwd
        ["-"] -> findConfigFile cwd
        file : _ -> findConfigFile $ FP.takeDirectory file

    output = hPutStrLn stderr
    outputError = output
    outputInfo = unless (optQuiet opts) . output
    outputDebug = when (cfgDebug cliConfig) . output

getHaskellFiles :: FilePath -> IO [FilePath]
getHaskellFiles input = do
  isDir <- doesDirectoryExist input
  if isDir
    then filter (".hs" `isSuffixOf`) <$> listDirectoryRecursive input
    else return [input] -- plain file
  where
    listDirectoryRecursive fp
      -- automatically ignore files in certain hardcoded directories
      -- TODO: make configurable?
      | FP.takeFileName fp `elem` [".stack-work", "dist", "dist-newstyle"] = pure []
      | otherwise = fmap concat . mapM (go . (fp FP.</>)) =<< listDirectory fp
      where
        go child = do
          isDir <- doesDirectoryExist child
          if isDir
            then listDirectoryRecursive child
            else pure [child]

-- | Format a single input.
formatOne ::
  -- | How to use .cabal files
  ConfigFileOpts ->
  -- | Mode of operation
  Mode ->
  -- | The 'SourceType' requested by the user
  Maybe SourceType ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ExitCode
formatOne ConfigFileOpts {..} mode reqSourceType rawConfig mpath =
  withPrettyOrmoluExceptions (cfgColorMode rawConfig) $ do
    let getCabalInfoForSourceFile' sourceFile = do
          cabalSearchResult <- getCabalInfoForSourceFile sourceFile
          let debugEnabled = cfgDebug rawConfig
          case cabalSearchResult of
            CabalNotFound -> do
              when debugEnabled $
                hPutStrLn stderr $
                  "Could not find a .cabal file for " <> sourceFile
              return Nothing
            CabalDidNotMention cabalInfo -> do
              when debugEnabled $ do
                relativeCabalFile <-
                  makeRelativeToCurrentDirectory (ciCabalFilePath cabalInfo)
                hPutStrLn stderr $
                  "Found .cabal file "
                    <> relativeCabalFile
                    <> ", but it did not mention "
                    <> sourceFile
              return (Just cabalInfo)
            CabalFound cabalInfo -> return (Just cabalInfo)
        getDotOrmoluForSourceFile' sourceFile = do
          if optDoNotUseDotOrmolu
            then return Nothing
            else Just <$> getDotOrmoluForSourceFile sourceFile
    case FP.normalise <$> mpath of
      -- input source = STDIN
      Nothing -> do
        mcabalInfo <- case (optStdinInputFile, optDoNotUseCabal) of
          (_, True) -> return Nothing
          (Nothing, False) -> throwIO OrmoluMissingStdinInputFile
          (Just inputFile, False) -> getCabalInfoForSourceFile' inputFile
        mdotOrmolu <- case optStdinInputFile of
          Nothing -> return Nothing
          Just inputFile -> getDotOrmoluForSourceFile' inputFile
        config <- patchConfig Nothing mcabalInfo mdotOrmolu
        case mode of
          Stdout -> do
            ormoluStdin config >>= T.Utf8.putStr
            return ExitSuccess
          InPlace -> do
            hPutStrLn
              stderr
              "In place editing is not supported when input comes from stdin."
            -- 101 is different from all the other exit codes we already use.
            return (ExitFailure 101)
          Check -> do
            -- ormoluStdin is not used because we need the originalInput
            originalInput <- T.Utf8.getContents
            let stdinRepr = "<stdin>"
            formattedInput <-
              ormolu config stdinRepr originalInput
            handleDiff originalInput formattedInput stdinRepr
      -- input source = a file
      Just inputFile -> do
        mcabalInfo <-
          if optDoNotUseCabal
            then return Nothing
            else getCabalInfoForSourceFile' inputFile
        mdotOrmolu <- getDotOrmoluForSourceFile' inputFile
        config <-
          patchConfig
            (Just (detectSourceType inputFile))
            mcabalInfo
            mdotOrmolu
        case mode of
          Stdout -> do
            ormoluFile config inputFile >>= T.Utf8.putStr
            return ExitSuccess
          InPlace -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- T.Utf8.readFile inputFile
            formattedInput <-
              ormolu config inputFile originalInput
            when (formattedInput /= originalInput) $
              T.Utf8.writeFile inputFile formattedInput
            return ExitSuccess
          Check -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- T.Utf8.readFile inputFile
            formattedInput <-
              ormolu config inputFile originalInput
            handleDiff originalInput formattedInput inputFile
  where
    patchConfig mdetectedSourceType mcabalInfo mdotOrmolu = do
      let sourceType =
            fromMaybe
              ModuleSource
              (reqSourceType <|> mdetectedSourceType)
      return $
        refineConfig
          sourceType
          mcabalInfo
          (Just (cfgFixityOverrides rawConfig))
          (Just (cfgModuleReexports rawConfig))
          ( rawConfig
              { cfgFixityOverrides = maybe defaultFixityOverrides fst mdotOrmolu,
                cfgModuleReexports = maybe defaultModuleReexports snd mdotOrmolu
              }
          )
    handleDiff originalInput formattedInput fileRepr =
      case diffText originalInput formattedInput fileRepr of
        Nothing -> return ExitSuccess
        Just diff -> do
          runTerm (printTextDiff diff) (cfgColorMode rawConfig) stderr
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          return (ExitFailure 100)

----------------------------------------------------------------------------
-- Command line options parsing

-- | All command line options.
data Opts = Opts
  { -- | Mode of operation
    optMode :: !Mode,
    -- | Whether to make the output quieter
    optQuiet :: !Bool,
    -- | Ormolu 'Config'
    optConfig :: !(Config RegionIndices),
    -- | Ormolu 'Config'
    optConfigFilePath :: !(Maybe FilePath),
    -- | Fourmolu 'PrinterOpts',
    optPrinterOpts :: PrinterOptsPartial,
    -- | Options related to info extracted from files
    optConfigFileOpts :: ConfigFileOpts,
    -- | Source type option, where 'Nothing' means autodetection
    optSourceType :: !(Maybe SourceType),
    -- | Haskell source files to format or stdin (when the list is empty)
    optInputFiles :: ![FilePath]
  }

-- | Mode of operation.
data Mode
  = -- | Output formatted source code to stdout
    Stdout
  | -- | Overwrite original file
    InPlace
  | -- | Exit with non-zero status code if
    -- source is not already formatted
    Check
  deriving (Eq, Show, Bounded, Enum)

-- | Options related to configuration stored in the file system.
data ConfigFileOpts = ConfigFileOpts
  { -- | DO NOT extract default-extensions and dependencies from .cabal files
    optDoNotUseCabal :: Bool,
    -- | DO NOT look for @.ormolu@ files
    optDoNotUseDotOrmolu :: Bool,
    -- | Optional path to a file which will be used to find a .cabal file
    -- when using input from stdin
    optStdinInputFile :: Maybe FilePath
  }
  deriving (Show)

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> exts <*> printDefaults <*> optsParser) . mconcat $
    [fullDesc]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      intercalate
        "\n"
        [ unwords $
            ["fourmolu", showVersion version]
              <> maybeToList $$(envQ @String "FOURMOLU_REV"),
          "using ghc-lib-parser " ++ VERSION_ghc_lib_parser
        ]
    exts :: Parser (a -> a)
    exts =
      infoOption displayExts . mconcat $
        [ long "manual-exts",
          help "Display extensions that need to be enabled manually"
        ]
    displayExts = unlines $ sort (showOutputable <$> manualExts)

    printDefaults :: Parser (a -> a)
    printDefaults =
      infoOption defaultPrinterOptsYaml . mconcat $
        [ long "print-defaults",
          help "Print default configuration options that can be used in fourmolu.yaml"
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( (fmap (bool Stdout InPlace) . switch . mconcat)
            [ short 'i',
              help "A shortcut for --mode inplace"
            ]
            <|> (option parseMode . mconcat)
              [ long "mode",
                short 'm',
                metavar "MODE",
                value Stdout,
                help "Mode of operation: 'stdout' (the default), 'inplace', or 'check'"
              ]
        )
    <*> (switch . mconcat)
      [ long "quiet",
        short 'q',
        help "Make output quieter"
      ]
    <*> configParser
    <*> (optional . strOption . mconcat)
      [ metavar "CONFIG_FILE",
        long "config",
        help "Path to the config file to use. If not specified, tries to discover one automatically."
      ]
    <*> printerOptsParser
    <*> configFileOptsParser
    <*> sourceTypeParser
    <*> (many . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source files to format or stdin (the default)"
      ]

configFileOptsParser :: Parser ConfigFileOpts
configFileOptsParser =
  ConfigFileOpts
    <$> (switch . mconcat)
      [ long "no-cabal",
        help "Do not extract default-extensions and dependencies from .cabal files"
      ]
    <*> pure True -- Fourmolu: Don't add the '--no-dot-ormolu' flag, hardcode to never looking for .ormolu files
    <*> (optional . strOption . mconcat)
      [ long "stdin-input-file",
        help "Path which will be used to find the .cabal file when using input from stdin"
      ]

configParser :: Parser (Config RegionIndices)
configParser =
  Config
    <$> (fmap (fmap DynOption) . many . strOption . mconcat)
      [ long "ghc-opt",
        short 'o',
        metavar "OPT",
        help "GHC options to enable (e.g. language extensions)"
      ]
    <*> ( fmap (FixityOverrides . Map.fromList . mconcat)
            . many
            . option parseFixityDeclaration
            . mconcat
        )
      [ long "fixity",
        short 'f',
        metavar "FIXITY",
        help "Fixity declaration to use (an override)"
      ]
    <*> ( fmap (ModuleReexports . Map.fromListWith (<>) . mconcat . pure)
            . many
            . option parseModuleReexportDeclaration
            . mconcat
        )
      [ long "reexport",
        short 'r',
        metavar "REEXPORT",
        help "Module re-export that Fourmolu should know about"
      ]
    <*> (fmap Set.fromList . many . strOption . mconcat)
      [ long "package",
        short 'p',
        metavar "PACKAGE",
        help "Explicitly specified dependency (for operator fixity/precedence only)"
      ]
    <*> (switch . mconcat)
      [ long "unsafe",
        short 'u',
        help "Do formatting faster but without automatic detection of defects"
      ]
    <*> (switch . mconcat)
      [ long "debug",
        short 'd',
        help "Output information useful for debugging"
      ]
    <*> (switch . mconcat)
      [ long "check-idempotence",
        short 'c',
        help "Fail if formatting is not idempotent"
      ]
    -- We cannot parse the source type here, because we might need to do
    -- autodection based on the input file extension (not available here)
    -- before storing the resolved value in the config struct.
    <*> pure ModuleSource
    <*> (option parseColorMode . mconcat)
      [ long "color",
        metavar "WHEN",
        value Auto,
        help "Colorize the output; WHEN can be 'never', 'always', or 'auto' (the default)"
      ]
    <*> ( RegionIndices
            <$> (optional . option auto . mconcat)
              [ long "start-line",
                metavar "START",
                help "Start line of the region to format (starts from 1)"
              ]
            <*> (optional . option auto . mconcat)
              [ long "end-line",
                metavar "END",
                help "End line of the region to format (inclusive)"
              ]
        )
    <*> pure defaultPrinterOpts -- unused; overwritten in resolveConfig
    <*> (fmap Set.fromList . many . strOption . mconcat)
      [ long "local-modules",
        short 'm',
        metavar "LOCAL_MODULES",
        help "Modules Fourmolu should consider as local by the current Cabal package"
      ]

sourceTypeParser :: Parser (Maybe SourceType)
sourceTypeParser =
  (option parseSourceType . mconcat)
    [ long "source-type",
      short 't',
      metavar "TYPE",
      value Nothing,
      help "Set the type of source; TYPE can be 'module', 'sig', or 'auto' (the default)"
    ]

printerOptsParser :: Parser PrinterOptsPartial
printerOptsParser = parsePrinterOptsCLI mkOption
  where
    mkOption name helpText placeholder =
      option (Just <$> eitherReader parsePrinterOptType) . mconcat $
        [ long name,
          help helpText,
          metavar placeholder,
          value Nothing
        ]

----------------------------------------------------------------------------
-- Helpers

-- | Parse 'Mode'.
parseMode :: ReadM Mode
parseMode = eitherReader $ \case
  "stdout" -> Right Stdout
  "inplace" -> Right InPlace
  "check" -> Right Check
  s -> Left $ "unknown mode: " ++ s

-- | Parse a fixity declaration.
parseFixityDeclaration :: ReadM [(OpName, FixityInfo)]
parseFixityDeclaration = eitherReader parseFixityDeclarationStr

-- | Parse a module reexport declaration.
parseModuleReexportDeclaration ::
  ReadM (ModuleName, NonEmpty (Maybe PackageName, ModuleName))
parseModuleReexportDeclaration = eitherReader parseModuleReexportDeclarationStr

-- | Parse 'ColorMode'.
parseColorMode :: ReadM ColorMode
parseColorMode = eitherReader $ \case
  "never" -> Right Never
  "always" -> Right Always
  "auto" -> Right Auto
  s -> Left $ "unknown color mode: " ++ s

-- | Parse the 'SourceType'. 'Nothing' means that autodetection based on
-- file extension is requested.
parseSourceType :: ReadM (Maybe SourceType)
parseSourceType = eitherReader $ \case
  "module" -> Right (Just ModuleSource)
  "sig" -> Right (Just SignatureSource)
  "auto" -> Right Nothing
  s -> Left $ "unknown source type: " ++ s
