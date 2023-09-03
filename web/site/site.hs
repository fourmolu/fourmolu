{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

import CMarkGFM.Simple qualified as CMarkGFM
import CMarkGFM.Simple.GhcSyntaxHighlighter qualified as CMarkGFM
import Control.Monad (forM, (<=<))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Version (showVersion)
import FourmoluConfig.ConfigData qualified as ConfigData
import FourmoluConfig.GenerateUtils (fieldTypesMap, getOptionSchema, hs2yaml)
import GHC.SyntaxHighlighter.Themed.HighlightJS qualified as HighlightJS
import Hakyll
import Hakyll.Core.Compiler.Internal (compilerUnsafeIO)
import Ormolu qualified as Fourmolu
import Paths_fourmolu qualified as Fourmolu
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, makeRelative, splitFileName, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Printf (printf)

main :: IO ()
main = hakyll $ do
  match "pages/**" $ do
    route $
      gsubRoute "pages/" (const "")
        `composeRoutes` setExtension "html"
        `composeRoutes` indexify
    compile $ do
      file <- makeRelative "./pages/" <$> getResourceFilePath
      let PageInfo {..} = getPageInfo file
      let context = pageContext <> foldMap makeSidebar pageSidebar <> baseContext
      markdownCompiler context processMarkdown
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  match "static/*" $ do
    route $ customRoute $ \ident ->
      case toFilePath ident of
        "static/favicon.ico" -> "favicon.ico"
        path -> path
    compile copyFileCompiler

  match "templates/*" $ compile templateBodyCompiler

baseContext :: Context String
baseContext =
  mconcat
    [ constField "version" fourmoluVersion,
      constField "commit" gitCommit,
      defaultContext
    ]

data PageInfo = PageInfo
  { pageContext :: Context String,
    pageSidebar :: Maybe PageSidebar
  }

data PageSidebar = PageSidebar
  { sidebarTitle :: Text,
    sidebarLinks :: [(Text, Text)]
  }

getPageInfo :: FilePath -> PageInfo
getPageInfo = \case
  "index.md" ->
    PageInfo
      { pageContext =
          listFieldString "demoOptions" "widget" $
            [ (ConfigData.name option, widget)
              | option <- ConfigData.allOptions,
                Just widget <- pure $ getOptionDemoWidget option
            ],
        pageSidebar = Nothing
      }
  "changelog.md" ->
    PageInfo
      { pageContext = constField "changelog" changelogContents,
        pageSidebar = Nothing
      }
  "config.md" ->
    PageInfo
      { pageContext =
          optionsField "options" . mconcat $
            [ field "name" (pure . ConfigData.name . itemBody),
              field "description" (pure . ConfigData.description . itemBody)
            ],
        pageSidebar = Nothing
      }
  fp
    | Just option <- getConfigOption fp ->
        PageInfo
          { pageContext = mconcat $ getConfigOptionContext option,
            pageSidebar =
              Just
                PageSidebar
                  { sidebarTitle = "Configuration options",
                    sidebarLinks =
                      [ (Text.pack name, "/config/" <> Text.pack name)
                        | ConfigData.Option {name} <- ConfigData.allOptions
                      ]
                  }
          }
  _ ->
    PageInfo
      { pageContext = mempty,
        pageSidebar = Nothing
      }
  where
    getConfigOption fp =
      case splitFileName fp of
        ("config/", pageFile)
          | let pageFileName = dropExtension pageFile,
            [option] <- filter ((== pageFileName) . ConfigData.name) ConfigData.allOptions ->
              Just option
        _ -> Nothing

    optionsField fieldName optionCtx =
      listField fieldName optionCtx . pure $
        [ Item (fromFilePath $ ConfigData.name option) option
          | option <- ConfigData.allOptions
        ]

getOptionDemoWidget :: ConfigData.Option -> Maybe String
getOptionDemoWidget option@ConfigData.Option {..}
  | name `elem` ["fixities", "reexports"] = Nothing
  | otherwise =
      Just . concat $
        [ printf "<label>",
          printf "  <code>%s</code>" name,
          optionDemoInput,
          printf "</label>"
        ]
  where
    ConfigData.ADTSchema {..} = getOptionSchema option
    inputDefault = hs2yaml type_ default_
    inputClass = "demo-printerOpt" :: String
    optionDemoInput =
      case adtInputType of
        ConfigData.ADTSchemaInputText parsers ->
          printf
            "<input class='%s' name='%s' type='text' value='%s' data-parsers='%s' />"
            inputClass
            name
            inputDefault
            (renderParsers parsers)
        ConfigData.ADTSchemaInputNumber ->
          printf
            "<input class='%s' name='%s' type='number' value='%s' />"
            inputClass
            name
            inputDefault
        ConfigData.ADTSchemaInputCheckbox ->
          printf
            "<input class='%s' name='%s' type='checkbox' %s />"
            inputClass
            name
            (if default_ == ConfigData.HsBool True then "checked" else "" :: String)
        ConfigData.ADTSchemaInputDropdown parsers ->
          concat
            [ printf
                "<select class='%s' name='%s' data-parsers='%s'>"
                inputClass
                name
                (renderParsers parsers),
              concat
                [ printf "<option %s>%s</option>" selected v
                  | opt <- adtOptions,
                    let v =
                          case opt of
                            ConfigData.ADTOptionLiteral s -> s
                            ConfigData.ADTOptionDescription s -> s,
                    let selected = if v == inputDefault then "selected" else "" :: String
                ],
              printf "</select>"
            ]
    renderParsers parsers =
      intercalate ("|" :: String) . flip map parsers $ \case
        ConfigData.ADTSchemaInputParserString -> "string"
        ConfigData.ADTSchemaInputParserNumber -> "number"
        ConfigData.ADTSchemaInputParserNull -> "null"

getConfigOptionContext :: ConfigData.Option -> [Context a]
getConfigOptionContext option@ConfigData.Option {..} =
  [ constField "info" $
      wrap' "<table id=\"config-info\">" "</table>" . wrap "tbody" . concat $
        [ wrap "tr" . concat $
            [ wrap "th" label,
              wrap "td" val
            ]
          | (label, val) <-
              [ ("Description", description),
                schema,
                ("Default", wrap "code" $ hs2yaml type_ default_),
                ("Ormolu", wrap "code" $ hs2yaml type_ ormolu),
                ("Since", maybe "<i>Unreleased</i>" ("v" <>) sinceVersion)
              ]
        ]
  ]
  where
    ConfigData.ADTSchema {adtOptions} = getOptionSchema option
    schema =
      if type_ `Map.member` fieldTypesMap
        then
          ( "Options",
            wrap "ul" . concat $
              [ wrap "li" (renderOptionHTML opt)
                | opt <- adtOptions
              ]
          )
        else
          ( "Type",
            wrap "code" type_
          )
    renderOptionHTML = \case
      ConfigData.ADTOptionLiteral s -> wrap "code" s
      ConfigData.ADTOptionDescription s -> s
    wrap tag = wrap' (printf "<%s>" (tag :: String)) (printf "</%s>" tag)
    wrap' pre post s = pre <> s <> post

makeSidebar :: PageSidebar -> Context String
makeSidebar PageSidebar {..} =
  mconcat
    [ boolField "sidebar" (const True),
      constField "sidebar.title" (Text.unpack sidebarTitle),
      listField
        "sidebar.links"
        ( mconcat
            [ field "name" (pure . Text.unpack . fst . itemBody),
              field "url" (pure . Text.unpack . snd . itemBody)
            ]
        )
        ( pure
            [ Item (fromFilePath $ Text.unpack name) (name, url)
              | (name, url) <- sidebarLinks
            ]
        )
    ]

processMarkdown :: CMarkGFM.Nodes -> IO CMarkGFM.Nodes
processMarkdown = fmap highlight . replaceFourmoluExamples
  where
    highlight = CMarkGFM.highlightHaskellWith HighlightJS.config

replaceFourmoluExamples :: CMarkGFM.Nodes -> IO CMarkGFM.Nodes
replaceFourmoluExamples =
  mapNodesM $ \nodes ->
    case breakMaybe getExampleInput nodes of
      (_, Nothing) -> pure nodes
      (before, Just (input, rest)) -> do
        let (rawTabs, after) = spanMaybe getExampleTab rest
        tabs <-
          forM rawTabs $ \rawTab ->
            case Text.breakOn "\n" rawTab of
              (label, rawPrinterOpts) -> do
                printerOpts <-
                  either error pure $
                    Aeson.eitherDecode . ByteString.fromStrict . Text.encodeUtf8 $
                      rawPrinterOpts
                let key = Text.toLower . Text.replace " " "-" $ label
                pure (label, key, printerOpts)
        outputs <-
          forM (withFirst tabs) $ \(isFirst, (label, key, printerOpts)) -> do
            let isActive = isFirst
            let config =
                  Fourmolu.defaultConfig
                    { Fourmolu.cfgPrinterOpts = Fourmolu.resolvePrinterOpts [printerOpts]
                    }
            output <- Fourmolu.ormolu config "<fourmolu-web>" input
            pure (isActive, label, key, output)
        pure . concat $
          [ before,
            [html "<ul class='nav nav-tabs' role='tablist'>"],
            concat
              [ [ html "<li class='nav-item' role='presentation'>",
                  html $ Text.pack $ printf "<button %s>%s</button>" attrs label,
                  html "</li>"
                ]
                | (isActive, label, key, _) <- outputs,
                  let attrs =
                        mkAttrs
                          [ ("class", "nav-link" <> if isActive then " active" else ""),
                            ("id", "example-tab-" <> key),
                            ("data-bs-toggle", "tab"),
                            ("data-bs-target", "#example-output-" <> key),
                            ("role", "tab"),
                            ("aria-controls", "example-output-" <> key)
                          ]
              ],
            [html "</ul>"],
            [html "<div class='tab-content'>"],
            concat
              [ [ html $ Text.pack $ printf "<div %s>" attrs,
                  noPos $ CMarkGFM.NodeCodeBlock "haskell" output,
                  html "</div>"
                ]
                | (isActive, _, key, output) <- outputs,
                  let attrs =
                        mkAttrs
                          [ ("class", "tab-pane" <> if isActive then " active" else ""),
                            ("id", "example-output-" <> key),
                            ("role", "tabpanel"),
                            ("aria-labelledby", "example-tab-" <> key),
                            ("tabindex", "0")
                          ]
              ],
            [html "</div>"],
            after
          ]
  where
    getExampleInput = \case
      CMarkGFM.WithPos _ (CMarkGFM.NodeCodeBlock "fourmolu-example-input" input) -> Just input
      _ -> Nothing
    getExampleTab = \case
      CMarkGFM.WithPos _ (CMarkGFM.NodeCodeBlock "fourmolu-example-tab" printerOpts) -> Just printerOpts
      _ -> Nothing

    noPos = CMarkGFM.WithPos Nothing
    html = noPos . CMarkGFM.NodeHtmlBlock
    mkAttrs = Text.pack . unwords . map (\(k, v) -> printf "%s='%s'" (k :: String) v)

    withFirst = zip (True : repeat False)

    breakMaybe :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
    breakMaybe f = go id
      where
        go !acc [] = (acc [], Nothing)
        go !acc (a : as) =
          case f a of
            Nothing -> go (acc . (a :)) as
            Just b -> (acc [], Just (b, as))

    spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
    spanMaybe _ [] = ([], [])
    spanMaybe f (a : as) =
      case f a of
        Nothing -> ([], a : as)
        Just b -> first (b :) $ spanMaybe f as

    -- ([WithPos (Node Nodes)] -> m [WithPos (Node Nodes)]) -> Nodes -> m Nodes
    mapNodesM f = CMarkGFM.overNodes $ \go -> fmap CMarkGFM.Nodes . traverse (traverse (sequence . go)) <=< f

{----- Data -----}

fourmoluVersion :: String
fourmoluVersion = showVersion Fourmolu.version

{-# NOINLINE gitCommit #-}
gitCommit :: String
gitCommit =
  unsafePerformIO $
    lookupEnv "FOURMOLU_REV" >>= \case
      Just rev -> pure rev
      Nothing -> strip <$> readProcess "git" ["rev-parse", "HEAD"] ""
  where
    strip = Text.unpack . Text.strip . Text.pack

{-# NOINLINE changelogContents #-}
changelogContents :: String
changelogContents = unsafePerformIO $ readFile "../../CHANGELOG.md"

{----- Utilities -----}

-- | Make the URL look like "/foo/bar", by changing the
-- "/foo/bar.html" path to "/foo/bar/index.html"
indexify :: Routes
indexify =
  customRoute $ \ident ->
    let path = toFilePath ident
     in case path of
          "index.html" -> path
          _ -> dropExtension path </> "index.html"

markdownCompiler :: Context String -> (CMarkGFM.Nodes -> IO CMarkGFM.Nodes) -> Compiler (Item String)
markdownCompiler ctx mdProcess =
  getResourceString
    >>= applyAsTemplate ctx
    >>= withItemBody (pure . mdParse . Text.pack)
    >>= withItemBody (compilerUnsafeIO . traverse mdProcess)
    >>= withItemBody (pure . Text.unpack . mdRender)
  where
    mdParse = CMarkGFM.parse CMarkGFM.defaultSettings
    mdRender = CMarkGFM.renderHtml

listFieldString :: String -> String -> [(String, String)] -> Context a
listFieldString listName itemName items =
  listField listName (field itemName (pure . itemBody)) $
    pure [Item (fromFilePath name) item | (name, item) <- items]
