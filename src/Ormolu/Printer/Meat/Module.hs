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

import Control.Monad
import Data.Choice (pattern With)
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
        | isOnSameLine am_mod am_where || isOnSameLine am_sig am_where = space
        | Just closeParen <- mCloseParen, isOnSameLine closeParen am_where = space
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
    AnnsModule {am_sig, am_mod, am_where} = anns hsmodAnn
    mCloseParen = do
      AnnList {al_brackets} <- anns . getLoc <$> hsmodExports
      case al_brackets of
        ListParens _ closeParen -> pure closeParen
        _ -> error "Unexpectedly got a different kind of bracket in module export list"
    isOnSameLine = curry $ \case
      (EpTok token1, EpTok token2) ->
        let loc1 = epaLocationRealSrcSpan token1
            loc2 = epaLocationRealSrcSpan token2
         in srcSpanEndLine loc1 == srcSpanStartLine loc2
      _ -> False
