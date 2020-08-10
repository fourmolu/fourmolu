{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Configuration options used by the tool.
module Ormolu.Config
  ( Config (..),
    RegionIndices (..),
    RegionDeltas (..),
    defaultConfig,
    PrinterOpts (..),
    PrinterOptsPartial,
    PrinterOptsTotal,
    defaultPrinterOpts,
    loadConfigFile,
    fillMissingPrinterOpts,
    regionIndicesToDeltas,
    DynOption (..),
    dynOptionToLocatedStr,
  )
where

import Control.Monad (when)
import Data.Aeson
  ( FromJSON (..),
    camelTo2,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    rejectUnknownFields,
  )
import Data.Functor.Identity (Identity (..))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.Generics (Generic)
import qualified SrcLoc as GHC
import System.Directory
  ( XdgDirectory (XdgConfig),
    findFile,
    getCurrentDirectory,
    getXdgDirectory,
    makeAbsolute,
  )
import System.FilePath (splitPath, (</>))
import System.IO (hPutStrLn, stderr)

-- | Ormolu configuration.
data Config region = Config
  { -- | Dynamic options to pass to GHC parser
    cfgDynOptions :: ![DynOption],
    -- | Do formatting faster but without automatic detection of defects
    cfgUnsafe :: !Bool,
    -- | Output information useful for debugging
    cfgDebug :: !Bool,
    -- | Checks if re-formatting the result is idempotent
    cfgCheckIdempotence :: !Bool,
    -- | Region selection
    cfgRegion :: !region,
    cfgPrinterOpts :: !PrinterOptsTotal
  }
  deriving (Eq, Show, Functor)

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
      cfgUnsafe = False,
      cfgDebug = False,
      cfgCheckIdempotence = False,
      cfgRegion =
        RegionIndices
          { regionStartLine = Nothing,
            regionEndLine = Nothing
          },
      cfgPrinterOpts = defaultPrinterOpts
    }

-- | Options controlling formatting output.
data PrinterOpts f = PrinterOpts
  { -- | Number of spaces to use for indentation
    poIndentation :: f Int
  }
  deriving (Generic)

-- | A version of 'PrinterOpts' where any field can be empty.
-- This corresponds to the information in a config file or in CLI options.
type PrinterOptsPartial = PrinterOpts Maybe

deriving instance Eq PrinterOptsPartial

deriving instance Show PrinterOptsPartial

instance Semigroup PrinterOptsPartial where
  (<>) = fillMissingPrinterOpts

instance Monoid PrinterOptsPartial where
  mempty = PrinterOpts {poIndentation = Nothing}

-- | A version of 'PrinterOpts' without empty fields.
type PrinterOptsTotal = PrinterOpts Identity

deriving instance Eq PrinterOptsTotal

deriving instance Show PrinterOptsTotal

defaultPrinterOpts :: PrinterOptsTotal
defaultPrinterOpts = PrinterOpts {poIndentation = Identity 4}

-- | Fill the field values that are 'Nothing' in the first argument
-- with the values of the corresponding fields of the second argument.
fillMissingPrinterOpts ::
  (Applicative f) =>
  PrinterOptsPartial ->
  PrinterOpts f ->
  PrinterOpts f
fillMissingPrinterOpts p1 p2 =
  PrinterOpts {poIndentation = fillField poIndentation poIndentation}
  where
    fillField f1 f2 =
      case f1 p1 of
        Nothing -> f2 p2
        Just x -> pure x

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

instance FromJSON PrinterOptsPartial where
  parseJSON =
    genericParseJSON
      defaultOptions
        { rejectUnknownFields = True,
          fieldLabelModifier = camelTo2 '_' . fromMaybe "" . stripPrefix "po"
        }

-- | Read options from a config file, if found.
-- Looks recursively in parent folders, then in 'XdgConfig',
-- for a file matching /fourmolu.yaml/.
loadConfigFile :: Bool -> Maybe FilePath -> IO PrinterOptsPartial
loadConfigFile debug maybePath = do
  root <- maybe getCurrentDirectory makeAbsolute maybePath
  xdg <- getXdgDirectory XdgConfig ""
  optsFromFile debug $ reverse $ xdg : scanl1 (</>) (splitPath root)

-- | Search the directories, in order, for a config file.
optsFromFile :: Bool -> [FilePath] -> IO PrinterOptsPartial
optsFromFile debug dirs =
  findFile dirs configFileName >>= \case
    Nothing -> do
      printDebug $
        "No " ++ show configFileName ++ " found in any of:\n"
          ++ unlines (map ("  " ++) dirs)
      return def
    Just file -> do
      printDebug $ "Found " ++ show file ++ ""
      decodeFileEither file >>= \case
        Left e -> do
          printDebug $ prettyPrintParseException e
          return def
        Right x -> return x
  where
    def = mempty
    printDebug = when debug . hPutStrLn stderr

configFileName :: FilePath
configFileName = "fourmolu.yaml"
