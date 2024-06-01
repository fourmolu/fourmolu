{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Configuration options used by the tool.
module Ormolu.Config
  ( Config (..),
    ColorMode (..),
    RegionIndices (..),
    RegionDeltas (..),
    SourceType (..),
    defaultConfig,
    overapproximatedDependencies,
    regionIndicesToDeltas,
    DynOption (..),
    dynOptionToLocatedStr,

    -- * Fourmolu configuration
    PrinterOpts (..),
    PrinterOptsPartial,
    PrinterOptsTotal,
    defaultPrinterOpts,
    defaultPrinterOptsYaml,
    fillMissingPrinterOpts,
    resolvePrinterOpts,
    CommaStyle (..),
    FunctionArrowsStyle (..),
    HaddockPrintStyle (..),
    HaddockPrintStyleModule (..),
    ImportExportStyle (..),
    LetStyle (..),
    InStyle (..),
    Unicode (..),
    ColumnLimit (..),
    SingleDerivingParens (..),
    parsePrinterOptsCLI,
    parsePrinterOptType,

    -- ** Loading Fourmolu configuration
    loadConfigFile,
    configFileName,
    FourmoluConfig (..),
    emptyConfig,
    ConfigFileLoadResult (..),
  )
where

import Control.Monad (forM)
import Data.Aeson ((.!=), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Yaml qualified as Yaml
import Distribution.Types.PackageName (PackageName)
import GHC.Generics (Generic)
import GHC.Types.SrcLoc qualified as GHC
import Ormolu.Config.Gen
import Ormolu.Fixity
import Ormolu.Terminal (ColorMode (..))
import Ormolu.Utils.Fixity (parseFixityDeclarationStr, parseModuleReexportDeclarationStr)
import System.Directory
  ( XdgDirectory (XdgConfig),
    findFile,
    getXdgDirectory,
    makeAbsolute,
  )
import System.FilePath (splitPath, (</>))
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

-- | Type of sources that can be formatted by Ormolu.
data SourceType
  = -- | Consider the input as a regular Haskell module
    ModuleSource
  | -- | Consider the input as a Backpack module signature
    SignatureSource
  deriving (Eq, Show)

-- | Ormolu configuration.
data Config region = Config
  { -- | Dynamic options to pass to GHC parser
    cfgDynOptions :: ![DynOption],
    -- | Fixity overrides
    cfgFixityOverrides :: !FixityOverrides,
    -- | Module reexports to take into account when doing fixity resolution
    cfgModuleReexports :: !ModuleReexports,
    -- | Known dependencies, if any
    cfgDependencies :: !(Set PackageName),
    -- | Do formatting faster but without automatic detection of defects
    cfgUnsafe :: !Bool,
    -- | Output information useful for debugging
    cfgDebug :: !Bool,
    -- | Checks if re-formatting the result is idempotent
    cfgCheckIdempotence :: !Bool,
    -- | How to parse the input (regular haskell module or Backpack file)
    cfgSourceType :: !SourceType,
    -- | Whether to use colors and other features of ANSI terminals
    cfgColorMode :: !ColorMode,
    -- | Region selection
    cfgRegion :: !region,
    cfgPrinterOpts :: !PrinterOptsTotal
  }
  deriving (Eq, Show, Functor, Generic)

-- | Region selection as the combination of start and end line numbers.
data RegionIndices = RegionIndices
  { -- | Start line of the region to format
    regionStartLine :: !(Maybe Int),
    -- | End line of the region to format
    regionEndLine :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Region selection as the length of the literal prefix and the literal
-- suffix.
data RegionDeltas = RegionDeltas
  { -- | Prefix length in number of lines
    regionPrefixLength :: !Int,
    -- | Suffix length in number of lines
    regionSuffixLength :: !Int
  }
  deriving (Eq, Show)

-- | Default @'Config' 'RegionIndices'@.
defaultConfig :: Config RegionIndices
defaultConfig =
  Config
    { cfgDynOptions = [],
      cfgFixityOverrides = defaultFixityOverrides,
      cfgModuleReexports = defaultModuleReexports,
      cfgDependencies = Set.empty,
      cfgUnsafe = False,
      cfgDebug = False,
      cfgCheckIdempotence = False,
      cfgSourceType = ModuleSource,
      cfgColorMode = Auto,
      cfgRegion =
        RegionIndices
          { regionStartLine = Nothing,
            regionEndLine = Nothing
          },
      cfgPrinterOpts = defaultPrinterOpts
    }

-- | Return all dependencies of the module. This includes both the declared
-- dependencies of the component we are working with and all potential
-- module re-export targets.
overapproximatedDependencies :: Config region -> Set PackageName
overapproximatedDependencies Config {..} =
  Set.union cfgDependencies potentialReexportTargets
  where
    potentialReexportTargets =
      Set.fromList
        . concatMap toTargetPackages
        $ Map.elems (unModuleReexports cfgModuleReexports)
    toTargetPackages = concatMap $ \case
      (Nothing, _) -> []
      (Just x, _) -> [x]

-- | Convert 'RegionIndices' into 'RegionDeltas'.
regionIndicesToDeltas ::
  -- | Total number of lines in the input
  Int ->
  -- | Region indices
  RegionIndices ->
  -- | Region deltas
  RegionDeltas
regionIndicesToDeltas total RegionIndices {..} =
  RegionDeltas
    { regionPrefixLength = maybe 0 (subtract 1) regionStartLine,
      regionSuffixLength = maybe 0 (total -) regionEndLine
    }

-- | A wrapper for dynamic options.
newtype DynOption = DynOption
  { unDynOption :: String
  }
  deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.
dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

----------------------------------------------------------------------------
-- Fourmolu configuration

-- | A version of 'PrinterOpts' where any field can be empty.
-- This corresponds to the information in a config file or in CLI options.
type PrinterOptsPartial = PrinterOpts Maybe

deriving instance Eq PrinterOptsPartial

deriving instance Show PrinterOptsPartial

instance Semigroup PrinterOptsPartial where
  (<>) = fillMissingPrinterOpts

instance Monoid PrinterOptsPartial where
  mempty = emptyPrinterOpts

instance Aeson.FromJSON PrinterOptsPartial where
  parseJSON =
    Aeson.withObject "PrinterOpts" $ \o ->
      parsePrinterOptsJSON (parseField o)
    where
      parseField :: (Aeson.FromJSON a) => Aeson.Object -> String -> Aeson.Parser (Maybe a)
      parseField o keyName = do
        let key = fromString keyName
        mValue <- o Aeson..:? key
        forM mValue $ \value ->
          Aeson.parseJSON value Aeson.<?> Aeson.Key key

-- | A version of 'PrinterOpts' without empty fields.
type PrinterOptsTotal = PrinterOpts Identity

deriving instance Eq PrinterOptsTotal

deriving instance Show PrinterOptsTotal

-- | Apply the given configuration in order (later options override earlier).
resolvePrinterOpts :: [PrinterOptsPartial] -> PrinterOptsTotal
resolvePrinterOpts = foldl' (flip fillMissingPrinterOpts) defaultPrinterOpts

----------------------------------------------------------------------------
-- Loading Fourmolu configuration

data FourmoluConfig = FourmoluConfig
  { cfgFilePrinterOpts :: PrinterOptsPartial,
    cfgFileFixities :: FixityOverrides,
    cfgFileReexports :: ModuleReexports
  }
  deriving (Eq, Show)

instance Aeson.FromJSON FourmoluConfig where
  parseJSON = Aeson.withObject "FourmoluConfig" $ \o -> do
    cfgFilePrinterOpts <- Aeson.parseJSON (Aeson.Object o)
    rawFixities <- o .:? "fixities" .!= []
    cfgFileFixities <-
      case mapM parseFixityDeclarationStr rawFixities of
        Right fixities -> return . FixityOverrides . Map.fromList . concat $ fixities
        Left e -> fail e
    rawReexports <- o .:? "reexports" .!= []
    cfgFileReexports <-
      case mapM parseModuleReexportDeclarationStr rawReexports of
        Right reexports -> return . ModuleReexports . Map.fromListWith (<>) $ reexports
        Left e -> fail e
    return FourmoluConfig {..}

emptyConfig :: FourmoluConfig
emptyConfig =
  FourmoluConfig
    { cfgFilePrinterOpts = mempty,
      cfgFileFixities = FixityOverrides mempty,
      cfgFileReexports = ModuleReexports mempty
    }

-- | Read options from a config file, if found.
--
-- Looks for a file named /fourmolu.yaml/, first in the given path and
-- its parents, and then in the XDG config directory.
loadConfigFile :: FilePath -> IO ConfigFileLoadResult
loadConfigFile rootDir = do
  rootDirAbs <- makeAbsolute rootDir
  xdg <- getXdgDirectory XdgConfig ""
  let dirs = reverse $ xdg : scanl1 (</>) (splitPath rootDirAbs)
  findFile dirs configFileName >>= \case
    Nothing -> return $ ConfigNotFound dirs
    Just file ->
      either (ConfigParseError file) (ConfigLoaded file)
        <$> Yaml.decodeFileEither file

-- | The result of calling 'loadConfigFile'.
data ConfigFileLoadResult
  = ConfigLoaded FilePath FourmoluConfig
  | ConfigParseError FilePath Yaml.ParseException
  | ConfigNotFound [FilePath]
  deriving (Show)

-- | Expected file name for YAML config.
configFileName :: FilePath
configFileName = "fourmolu.yaml"
