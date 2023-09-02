{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate, try)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe
  ( unsafePackCStringLen,
    unsafeUseAsCStringLen,
  )
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.Types.PackageName (PackageName)
import Foreign (Ptr)
import Foreign qualified
import Foreign.C.Types (CChar)
import ForeignUtils (alignmentFor, nextPtr, sizeFor)
import GHC.Generics (Generic)
import GHC.Hs.Dump qualified as GHC
import GHC.SyntaxHighlighter.Themed.HighlightJS qualified as HighlightJS
import GHC.Utils.Outputable qualified as GHC
import Ormolu (ormolu)
import Ormolu.Config (PrinterOptsPartial)
import Ormolu.Config qualified as Config
import Ormolu.Exception (printOrmoluException)
import Ormolu.Fixity qualified as Fixity
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result (ParseResult (..), SourceSnippet (..))
import Ormolu.Terminal (runTermPure)

foreign export ccall
  runFourmolu :: Ptr CChar -> Int -> IO (Ptr StringWithLen)

foreign export ccall
  evaluateFixityInfo :: IO ()

foreign export ccall
  getString :: Ptr StringWithLen -> IO (Ptr CChar)

foreign export ccall
  getStringLen :: Ptr StringWithLen -> IO Int

foreign export ccall
  freeStringWithLen :: Ptr StringWithLen -> IO ()

main :: IO ()
main = pure ()

-- | Run fourmolu with the given input, which is a String containing
-- a JSON encoding of 'Input'. Returns a String containing a JSON encoding
-- of 'Output'.
runFourmolu :: Ptr CChar -> Int -> IO (Ptr StringWithLen)
runFourmolu inputPtr inputLen = do
  result <-
    (`catch` pure . mkFatalOutputError) $ do
      inputBytes <- unsafePackCStringLen (inputPtr, inputLen)
      case Aeson.eitherDecodeStrict' inputBytes of
        Right input -> format input
        Left e -> error $ "Could not decode input: " ++ show e

  let outputBytes = ByteString.toStrict $ Aeson.encode result
  unsafeUseAsCStringLen outputBytes (uncurry mallocStringWithLen)
  where
    -- Uncaught errors aren't propagated to the browser, so
    -- this is a stop-gap to capture absolutely every error and
    -- send back the error without any other information
    mkFatalOutputError e =
      Output
        { outputHTML = "",
          inputAST = "",
          outputAST = "",
          formatError = Just . Text.pack . show $ (e :: SomeException)
        }

-- See comments in Ormolu.Fixity
evaluateFixityInfo :: IO ()
evaluateFixityInfo =
  void . evaluate $ force (Fixity.hackageInfo, demoDependencies)

{----- StringWithLen -----}

newtype StringWithLen = StringWithLen {unStringWithLen :: (Ptr CChar, Int)}

instance Foreign.Storable StringWithLen where
  sizeOf _ =
    aligned @StringWithLen
      . addSize @Int
      . addSize @(Ptr CChar)
      $ 0
    where
      addSize :: forall a. (Foreign.Storable a) => Int -> Int
      addSize x = aligned @a (x + sizeFor @a)

      aligned :: forall a. (Foreign.Storable a) => Int -> Int
      aligned = (`pad` alignmentFor @a)

      pad x n
        | x `mod` n == 0 = x
        | otherwise = pad (x + 1) n

  alignment _ =
    maximum
      [ alignmentFor @(Ptr CChar),
        alignmentFor @Int
      ]

  peek stringWithLenPtr = do
    stringPtr <- Foreign.peek stringPtrPtr
    len <- Foreign.peek lenPtr
    pure $ StringWithLen (stringPtr, len)
    where
      stringPtrPtr = Foreign.castPtr stringWithLenPtr :: Ptr (Ptr CChar)
      lenPtr = nextPtr stringPtrPtr :: Ptr Int

  poke stringWithLenPtr (StringWithLen (stringPtr, len)) = do
    Foreign.poke stringPtrPtr stringPtr
    Foreign.poke lenPtr len
    where
      stringPtrPtr = Foreign.castPtr stringWithLenPtr :: Ptr (Ptr CChar)
      lenPtr = nextPtr stringPtrPtr :: Ptr Int

getString :: Ptr StringWithLen -> IO (Ptr CChar)
getString = fmap (fst . unStringWithLen) . Foreign.peek

getStringLen :: Ptr StringWithLen -> IO Int
getStringLen = fmap (snd . unStringWithLen) . Foreign.peek

mallocStringWithLen :: Ptr CChar -> Int -> IO (Ptr StringWithLen)
mallocStringWithLen buf len = do
  stringPtr <- Foreign.mallocBytes len
  Foreign.copyBytes stringPtr buf len
  structPtr <- Foreign.malloc
  Foreign.poke structPtr $ StringWithLen (stringPtr, len)
  pure structPtr

freeStringWithLen :: Ptr StringWithLen -> IO ()
freeStringWithLen stringWithLenPtr = do
  Foreign.free =<< getString stringWithLenPtr
  Foreign.free stringWithLenPtr

{----- Format -----}

data Input = Input
  { inputText :: Text,
    printerOpts :: PrinterOptsPartial,
    checkIdempotence :: Bool,
    unsafeMode :: Bool,
    formatBackpack :: Bool
  }
  deriving (Show, Generic, Aeson.FromJSON)

data Output = Output
  { outputHTML :: Text,
    inputAST :: Text,
    outputAST :: Text,
    formatError :: Maybe Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

format :: Input -> IO Output
format Input {..} = do
  (outputText, formatError) <-
    try (ormolu config "<interactive>" inputText) >>= \case
      Right outputText -> pure (outputText, Nothing)
      Left e -> do
        let msg = runTermPure $ printOrmoluException e
        pure ("", Just msg)
  let outputHTML =
        fromMaybe ("<pre>" <> outputText <> "</pre>") $
          HighlightJS.renderHaskell outputText
  inputAST <- prettyAST config inputText
  outputAST <- prettyAST config outputText
  pure Output {..}
  where
    config =
      -- TODO: add fixities?
      Config.defaultConfig
        { Config.cfgPrinterOpts = Config.resolvePrinterOpts [] [printerOpts],
          Config.cfgCheckIdempotence = checkIdempotence,
          Config.cfgUnsafe = unsafeMode,
          Config.cfgSourceType = if formatBackpack then Config.SignatureSource else Config.ModuleSource,
          Config.cfgDependencies = demoDependencies
        }

prettyAST :: Config.Config Config.RegionIndices -> Text -> IO Text
prettyAST config text = do
  let fixityInfo = Fixity.packageFixityMap Fixity.defaultDependencies
  (_, snippets) <-
    parseModule
      (Config.regionIndicesToDeltas (length $ Text.lines text) <$> config)
      fixityInfo
      "<interactive>"
      text
  pure $ either (const "") (Text.unlines . map showSnippet) snippets
  where
    showSnippet = \case
      RawSnippet s -> s
      ParsedSnippet ParseResult {..} ->
        Text.pack
          . GHC.showSDocUnsafe
          . GHC.showAstData GHC.NoBlankSrcSpan GHC.NoBlankEpAnnotations
          $ prParsedSource

-- | We want to make as many packages as possible available by default, so we
-- only exclude packages that contain modules with the same name as in certain
-- "priority" packages, in order to avoid imprecise fixities.
demoDependencies :: Set PackageName
demoDependencies = Map.keysSet $ Map.filterWithKey nonConflicting hackageInfo
  where
    Fixity.HackageInfo hackageInfo = Fixity.hackageInfo
    priorityPkgs = Set.fromList ["base", "lens"]
    priorityModules =
      Set.unions . fmap Map.keysSet $
        Map.restrictKeys hackageInfo priorityPkgs
    nonConflicting pkgName modulesToInfo =
      pkgName `Set.member` priorityPkgs
        || Map.keysSet modulesToInfo `Set.disjoint` priorityModules
