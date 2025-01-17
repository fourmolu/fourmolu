{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Applicative
import Control.Monad
import Data.Choice (pattern With)
import Data.Maybe
import GHC.Driver.Ppr
import GHC.Hs hiding (comment)
import GHC.Types.SrcLoc
import Ormolu.Config
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma

-- | Render a module-like entity (either a regular module or a backpack
-- signature).
p_hsModule ::
  -- | Stack header
  Maybe LComment ->
  -- | Pragmas and the associated comments
  [([LComment], Pragma)] ->
  -- | AST to print
  HsModule GhcPs ->
  R ()
p_hsModule mstackHeader pragmas hsmod@HsModule {..} = do
  let XModulePs {..} = hsmodExt
      deprecSpan = maybe [] (pure . getLocA) hsmodDeprecMessage
      exportSpans = maybe [] (pure . getLocA) hsmodExports
  switchLayout (deprecSpan <> exportSpans) $ do
    forM_ mstackHeader $ \(L spn comment) -> do
      spitCommentNow spn comment
      newline
    newline
    p_pragmas pragmas
    newline
    mapM_ (p_hsModuleHeader hsmod) hsmodName
    newline
    respectful <- getPrinterOpt poRespectful
    localModules <- getLocalModules
    importGrouping <- getPrinterOpt poImportGrouping
    forM_ (normalizeImports respectful localModules importGrouping hsmodImports) $ \importGroup -> do
      forM_ importGroup (located' p_hsmodImport)
      newline
    declNewline
    switchLayout (getLocA <$> hsmodDecls) $ do
      preserveSpacing <- getPrinterOpt poRespectful
      (if preserveSpacing then p_hsDeclsRespectGrouping else p_hsDecls) Free hsmodDecls
      newline
      spitRemainingComments

p_hsModuleHeader :: HsModule GhcPs -> LocatedA ModuleName -> R ()
p_hsModuleHeader HsModule {hsmodExt = XModulePs {..}, ..} moduleName = do
  located moduleName $ \name -> do
    poHStyle <-
      getPrinterOpt poHaddockStyleModule >>= \case
        PrintStyleInherit -> getPrinterOpt poHaddockStyle
        PrintStyleOverride style -> pure style
    forM_ hsmodHaddockModHeader (p_hsDoc' poHStyle Pipe (With #endNewline))
    p_hsmodName name

  forM_ hsmodDeprecMessage $ \w -> do
    breakpoint
    located' p_warningTxt w

  isRespectful <- getPrinterOpt poRespectful
  isDiffFriendly <- (== ImportExportDiffFriendly) <$> getPrinterOpt poImportExportStyle
  let breakpointBeforeExportList =
        case (hsmodDeprecMessage, hsmodExports) of
          _ | not isDiffFriendly -> breakpoint
          (Nothing, _) -> space
          (Just _, Just exports) | (not . isOneLineSpan) (getLocA exports) -> space
          _ -> breakpoint
      breakpointBeforeWhere
        | not isRespectful = breakpointBeforeExportList
        | isOnSameLine moduleStartRealSpan moduleEndRealSpan = space
        | Just closeParen <- exportClosePSpan, isOnSameLine closeParen moduleEndRealSpan = space
        | otherwise = newline

  case hsmodExports of
    Nothing -> return ()
    Just l -> do
      breakpointBeforeExportList
      encloseLocated l $ \exports -> do
        inci (p_hsmodExports exports)

  breakpointBeforeWhere
  txt "where"
  newline
  where
    -- `epaLocationRealSrcSpan` throw exception, so I'm avoid it.
    moduleStartRealSpan =
      -- Do not execute functions for `UnhelpfulSpan`.
      -- There may be room for other exceptions just by missing them.
      let safeEpaLocationRealSrcSpan
            (EpaDelta (UnhelpfulSpan _) _ _) = Nothing
          safeEpaLocationRealSrcSpan
            loc = Just $ epaLocationRealSrcSpan loc
          moduleSpan moduleToToken = safeEpaLocationRealSrcSpan $ getEpTokenLoc $ moduleToToken $ anns hsmodAnn
          sigLoc = moduleSpan am_sig
          modLoc = moduleSpan am_mod
       in fromMaybe (error $ "Module had unexpected annotations: " ++ showPprUnsafe modLoc) $ sigLoc <|> modLoc
    moduleEndRealSpan = epaLocationRealSrcSpan $ getEpTokenLoc $ am_where $ anns hsmodAnn
    exportClosePSpan = do
      exports <- hsmodExports
      loc <- case al_brackets $ anns $ getLoc exports of
        ListParens _ closeToken -> Just $ getEpTokenLoc closeToken
        ListBraces _ closeToken -> Just $ getEpTokenLoc closeToken
        ListSquare _ closeToken -> Just $ getEpTokenLoc closeToken
        ListBanana _ closeToken -> Just $ getEpUniTokenLoc closeToken
        ListNone -> Nothing
      Just $ epaLocationRealSrcSpan loc
    isOnSameLine token1 token2 = srcSpanEndLine token1 == srcSpanStartLine token2
