{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import GHC.Hs hiding (comment)
import GHC.Types.SrcLoc
import GHC.Unit.Module.Name
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
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
  Maybe (RealLocated Comment) ->
  -- | Pragmas and the associated comments
  [([RealLocated Comment], Pragma)] ->
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
    preserveGroups <- getPrinterOpt poRespectful
    forM_ (normalizeImports preserveGroups hsmodImports) $ \importGroup -> do
      forM_ importGroup (located' p_hsmodImport)
      newline
    declNewline
    switchLayout (getLocA <$> hsmodDecls) $ do
      preserveSpacing <- getPrinterOpt poRespectful
      (if preserveSpacing then p_hsDeclsRespectGrouping else p_hsDecls) Free hsmodDecls
      newline
      spitRemainingComments

p_hsModuleHeader :: HsModule -> LocatedA ModuleName -> R ()
p_hsModuleHeader HsModule {..} moduleName = do
  located moduleName $ \name -> do
    poHStyle <-
      getPrinterOpt poHaddockStyleModule >>= \case
        PrintStyleInherit -> getPrinterOpt poHaddockStyle
        PrintStyleOverride style -> pure style
    forM_ hsmodHaddockModHeader (p_hsDoc' poHStyle Pipe True)
    p_hsmodName name

  forM_ hsmodDeprecMessage $ \w -> do
    breakpoint
    located' p_moduleWarning w

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
        | isOnSameLine moduleKeyword whereKeyword = space
        | Just closeParen <- exportClosePSpan, isOnSameLine closeParen whereKeyword = space
        | otherwise = newline

  case hsmodExports of
    Nothing -> return ()
    Just l -> do
      breakpointBeforeExportList
      located l $ \exports -> do
        inci (p_hsmodExports exports)

  breakpointBeforeWhere
  txt "where"
  newline
  where
    (moduleKeyword, whereKeyword) =
      case am_main (anns hsmodAnn) of
        -- [AnnModule, AnnWhere] or [AnnSignature, AnnWhere]
        [AddEpAnn _ moduleLoc, AddEpAnn AnnWhere whereLoc] ->
          (epaLocationRealSrcSpan moduleLoc, epaLocationRealSrcSpan whereLoc)
        anns -> error $ "Module had unexpected annotations: " ++ showSDocUnsafe (ppr anns)
    exportClosePSpan = do
      AddEpAnn AnnCloseP loc <- al_close . anns . ann . getLoc =<< hsmodExports
      Just $ epaLocationRealSrcSpan loc
    isOnSameLine token1 token2 = srcSpanEndLine token1 == srcSpanStartLine token2
