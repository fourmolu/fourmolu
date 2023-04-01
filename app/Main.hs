{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad
import Data.Bool (bool)
import Data.List (intercalate, isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml
import Language.Haskell.TH.Env (envQ)
import Options.Applicative
import Ormolu
import Ormolu.Config
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Fixity (FixityInfo, OpName)
import Ormolu.Parser (manualExts)
import Ormolu.Terminal
import Ormolu.Utils (showOutputable)
import Ormolu.Utils.Fixity (parseFixityDeclarationStr)
import Ormolu.Utils.IO
import Paths_fourmolu (version)
import System.Directory
import System.Exit (ExitCode (..), exitWith)
import qualified System.FilePath as FP
import System.IO (hPutStrLn, stderr)

-- | Entry point of the program.
main :: IO ()
main = do
  opts@Opts {..} <- execParser optsParserInfo

  cwd <- getCurrentDirectory
  cfg <- case optInputFiles of
    [] -> mkConfig cwd opts
    ["-"] -> mkConfig cwd opts
    file : _ -> mkConfig file opts

  let formatOne' =
        formatOne
          optCabal
          optMode
          optSourceType
          cfg

      getHaskellFiles input = do
        isDir <- doesDirectoryExist input
        if isDir
          then filter (".hs" `isSuffixOf`) <$> listDirectoryRecursive input
          else return [input] -- plain file
      listDirectoryRecursive fp = fmap concat . mapM (go . (fp FP.</>)) =<< listDirectory fp
        where
          go child = do
            isDir <- doesDirectoryExist child
            if isDir
              then listDirectoryRecursive child
              else pure [child]

      selectFailure = \case
        ExitSuccess -> Nothing
        ExitFailure n -> Just n

      formatInputs inputs = do
        files <- Set.toAscList . Set.fromList . concat <$> mapM getHaskellFiles inputs
        errorCodes <- mapMaybe selectFailure <$> mapM (formatOne' . Just) files
        return $
          if null errorCodes
            then ExitSuccess
            else
              ExitFailure $
                if all (== 100) errorCodes
                  then 100
                  else 102

  exitCode <- case optInputFiles of
    [] -> formatOne' Nothing
    ["-"] -> formatOne' Nothing
    xs -> formatInputs xs

  exitWith exitCode

-- | Build the full config, by adding 'PrinterOpts' from a file, if found.
mkConfig :: FilePath -> Opts -> IO (Config RegionIndices)
mkConfig path Opts {optQuiet, optConfig, optPrinterOpts = cliPrinterOpts} = do
  mFourmoluConfig <-
    loadConfigFile path >>= \case
      ConfigLoaded f cfg -> do
        unless optQuiet $
          hPutStrLn stderr $
            "Loaded config from: " <> f
        printDebug $ show cfg
        return $ Just cfg
      ConfigParseError f e -> do
        hPutStrLn stderr $
          unlines
            [ "Failed to load " <> f <> ":",
              Yaml.prettyPrintParseException e
            ]
        exitWith $ ExitFailure 400
      ConfigNotFound searchDirs -> do
        printDebug
          . unlines
          $ ("No " ++ show configFileName ++ " found in any of:")
            : map ("  " ++) searchDirs
        return Nothing
  let resolve f = maybe mempty f mFourmoluConfig
  return $
    optConfig
      { cfgPrinterOpts =
          fillMissingPrinterOpts cliPrinterOpts
            . fillMissingPrinterOpts (resolve cfgFilePrinterOpts)
            $ defaultPrinterOpts,
        cfgFixityOverrides =
          -- cfgFileFixities should go on the right so that command line
          -- fixity overrides takes precedence.
          cfgFixityOverrides optConfig <> resolve cfgFileFixities
      }
  where
    printDebug = when (cfgDebug optConfig) . hPutStrLn stderr

-- | Format a single input.
formatOne ::
  -- | How to use .cabal files
  CabalOpts ->
  -- | Mode of operation
  Mode ->
  -- | The 'SourceType' requested by the user
  Maybe SourceType ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ExitCode
formatOne CabalOpts {..} mode reqSourceType rawConfig mpath =
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
    case FP.normalise <$> mpath of
      -- input source = STDIN
      Nothing -> do
        resultConfig <-
          ( if optDoNotUseCabal
              then pure Nothing
              else case optStdinInputFile of
                Just stdinInputFile ->
                  getCabalInfoForSourceFile' stdinInputFile
                Nothing -> throwIO OrmoluMissingStdinInputFile
            )
            >>= patchConfig Nothing
        case mode of
          Stdout -> do
            ormoluStdin resultConfig >>= TIO.putStr
            return ExitSuccess
          InPlace -> do
            hPutStrLn
              stderr
              "In place editing is not supported when input comes from stdin."
            -- 101 is different from all the other exit codes we already use.
            return (ExitFailure 101)
          Check -> do
            -- ormoluStdin is not used because we need the originalInput
            originalInput <- getContentsUtf8
            let stdinRepr = "<stdin>"
            formattedInput <-
              ormolu resultConfig stdinRepr originalInput
            handleDiff originalInput formattedInput stdinRepr
      -- input source = a file
      Just inputFile -> do
        resultConfig <-
          ( if optDoNotUseCabal
              then pure Nothing
              else getCabalInfoForSourceFile' inputFile
            )
            >>= patchConfig (Just (detectSourceType inputFile))
        case mode of
          Stdout -> do
            ormoluFile resultConfig inputFile >>= TIO.putStr
            return ExitSuccess
          InPlace -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- readFileUtf8 inputFile
            formattedInput <-
              ormolu resultConfig inputFile originalInput
            when (formattedInput /= originalInput) $
              writeFileUtf8 inputFile formattedInput
            return ExitSuccess
          Check -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- readFileUtf8 inputFile
            formattedInput <-
              ormolu resultConfig inputFile originalInput
            handleDiff originalInput formattedInput inputFile
  where
    patchConfig mdetectedSourceType mcabalInfo = do
      let sourceType =
            fromMaybe
              ModuleSource
              (reqSourceType <|> mdetectedSourceType)
      mfixityOverrides <- traverse getFixityOverridesForSourceFile mcabalInfo
      return (refineConfig sourceType mcabalInfo mfixityOverrides rawConfig)
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
    -- | Fourmolu 'PrinterOpts',
    optPrinterOpts :: PrinterOptsPartial,
    -- | Options related to info extracted from .cabal files
    optCabal :: CabalOpts,
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

-- | Configuration related to .cabal files.
data CabalOpts = CabalOpts
  { -- | DO NOT extract default-extensions and dependencies from .cabal files
    optDoNotUseCabal :: Bool,
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
    <*> printerOptsParser
    <*> cabalOptsParser
    <*> sourceTypeParser
    <*> (many . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source files to format or stdin (the default)"
      ]

cabalOptsParser :: Parser CabalOpts
cabalOptsParser =
  CabalOpts
    <$> (switch . mconcat)
      [ long "no-cabal",
        help "Do not extract default-extensions and dependencies from .cabal files"
      ]
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
    <*> ( fmap (Map.fromListWith (<>) . mconcat)
            . many
            . option parseFixityDeclaration
            . mconcat
        )
      [ long "fixity",
        short 'f',
        metavar "FIXITY",
        help "Fixity declaration to use (an override)"
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
    <*> pure defaultPrinterOpts -- unused; overwritten in mkConfig

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
