{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A Fourmolu-specific module for rendering function-like type signatures.
module Ormolu.Printer.Meat.Type.Function
  ( -- * PrintHsFun
    PrintHsFun,
    MonadR,

    -- * ParsedFunRepr
    ParsedFunRepr,
    ParsedFunRepr' (..),
    FunRepr (..),
    p_hsFun,
    p_hsFunParsed,

    -- * forall
    ForAllVisibility (..),
    p_forallBndrs,

    -- * TyVarBndr
    p_hsTyVarBndr,

    -- * Contexts
    p_hsContext',

    -- * Haddocks
    withHaddocks,
  )
where

import Control.Monad (forM_, void, when)
import Control.Monad.Cont qualified as Cont
import Control.Monad.State qualified as State
import Control.Monad.Trans qualified as Trans
import Data.Choice (Choice, pattern Is, pattern Isn't, pattern With, pattern Without)
import Data.Choice qualified as Choice
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (isJust)
import GHC.Hs
import GHC.Types.SrcLoc (GenLocated (..))
import GHC.Types.Var (Specificity (..))
import GHC.Utils.Outputable (Outputable)
import Ormolu.Config
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common (p_arrow, p_hsDoc, p_rdrName)
import Ormolu.Utils (showOutputable)
import Prelude hiding (span)

{----- PrintHsFun -----}

newtype PrintHsFun a = PrintHsFun (State.StateT PrintHsFunState (Cont.ContT () R) a)
  deriving newtype (Functor, Applicative, Monad)

data PrintHsFunState = PrintHsFunState
  { -- | The leading delimiter to output at the next line
    leadingDelim :: Maybe (R ()),
    forceMultiline :: Bool,
    -- | Whether we're currently printing args
    inArgList :: Bool
  }

runPrintHsFun :: PrintHsFun () -> R ()
runPrintHsFun (PrintHsFun m) = Cont.evalContT . (`State.evalStateT` initialState) $ m
  where
    initialState =
      PrintHsFunState
        { leadingDelim = Nothing,
          forceMultiline = False,
          inArgList = False
        }

class (Monad m) => MonadR m where
  liftR :: R a -> m a

instance MonadR R where
  liftR = id

instance MonadR PrintHsFun where
  liftR = PrintHsFun . Trans.lift . Trans.lift

withApplyLeadingDelim :: (Maybe (R ()) -> PrintHsFun a) -> PrintHsFun a
withApplyLeadingDelim f = do
  state <- PrintHsFun State.get
  a <- f state.leadingDelim
  PrintHsFun . State.put $ state {leadingDelim = Nothing}
  pure a

applyLeadingDelim :: PrintHsFun ()
applyLeadingDelim = withApplyLeadingDelim (traverse_ liftR)

setLeadingDelim :: R () -> PrintHsFun ()
setLeadingDelim delim =
  PrintHsFun . State.modify $ \state -> state {leadingDelim = Just delim}

getIsMultiline :: PrintHsFun Bool
getIsMultiline = do
  state <- PrintHsFun State.get
  layout <- liftR getLayout
  pure $ state.forceMultiline || layout == MultiLine

setMultilineContext :: ParsedFunRepr' sig ctx arg arr ret -> PrintHsFun ()
setMultilineContext fun =
  PrintHsFun . State.modify $ \state -> state {forceMultiline = hasDocs fun}

getIsTrailing :: (MonadR m) => m (Choice "argDelim" -> Bool)
getIsTrailing = do
  arrowsStyle <- liftR $ getPrinterOpt poFunctionArrows
  pure $ \isArgDelim ->
    case arrowsStyle of
      TrailingArrows -> True
      LeadingArgsArrows -> not $ Choice.isTrue isArgDelim
      LeadingArrows -> False

getInArgList :: PrintHsFun Bool
getInArgList = PrintHsFun . State.gets $ (.inArgList)

setInArgList :: Bool -> PrintHsFun ()
setInArgList x = PrintHsFun . State.modify $ \state -> state {inArgList = x}

-- Make everything afterwards occur within a located block, with the
-- magic of ContT. `item <- setLocated litem; ...` is equivalent to
-- `located litem $ \item -> ...`.
setLocated :: (HasLoc ann) => GenLocated ann x -> PrintHsFun x
setLocated litem = PrintHsFun . Trans.lift $ Cont.ContT (located litem)

setLocated_ :: (HasLoc ann) => GenLocated ann x -> PrintHsFun ()
setLocated_ = void . setLocated

{----- ParsedFunRepr -----}

type ParsedFunRepr a =
  ParsedFunRepr'
    (FunReprSig a)
    (FunReprCtx a)
    (FunReprArg a)
    (FunReprArr a)
    (FunReprRet a)

-- | The parsed representation of a function
data ParsedFunRepr' sig ctx arg arr ret
  = -- | The "::" delimiter. Invariant: must be first.
    ParsedFunSig
      { sig :: sig,
        next :: ParsedFunRepr' sig ctx arg arr ret
      }
  | -- | The "forall a b." or "forall a b ->" construct.
    ParsedFunForall
      { tele :: LocatedA (HsForAllTelescope GhcPs),
        next :: ParsedFunRepr' sig ctx arg arr ret
      }
  | -- | The "(A, B) =>" construct.
    ParsedFunQuals
      { ctxs :: [LocatedA (LocatedC [LocatedA ctx])],
        next :: ParsedFunRepr' sig ctx arg arr ret
      }
  | ParsedFunArg
      { span :: SrcSpanAnnA,
        arg :: LocatedA arg,
        doc :: Maybe (LHsDoc GhcPs),
        arrow :: HsArrowOf (LocatedA arr) GhcPs,
        next :: ParsedFunRepr' sig ctx arg arr ret
      }
  | ParsedFunReturn
      { ret :: LocatedA ret,
        doc :: Maybe (LHsDoc GhcPs)
      }

class
  ( Anno (FunReprCtx a) ~ SrcSpanAnnA,
    Outputable (FunReprCtx a)
  ) =>
  FunRepr a
  where
  parseFunRepr :: LocatedA a -> ParsedFunRepr a

  type FunReprSig a
  type FunReprSig a = ()
  renderFunReprSig :: FunReprSig a -> R ()
  default renderFunReprSig :: (FunReprSig a ~ ()) => FunReprSig a -> R ()
  renderFunReprSig () = space *> token'dcolon

  type FunReprCtx a
  type FunReprCtx a = a
  renderFunReprCtx :: FunReprCtx a -> R ()

  type FunReprArg a
  type FunReprArg a = a
  renderFunReprArg :: FunReprArg a -> R ()

  type FunReprArr a
  type FunReprArr a = a
  renderFunReprArr :: FunReprArr a -> R ()

  type FunReprRet a
  type FunReprRet a = a
  renderFunReprRet :: FunReprRet a -> R ()

-- | Some functions rely on HsType having a FunRepr instance, but the instance
-- can't be implemented here, since p_hsType is implemented in Type.hs, which
-- would cause a circular dependency. So we'll add it as an explicit constraint
-- and the call-site should bring the FunRepr instance for HsType in scope.
type FunReprHsType =
  ( FunRepr (HsType GhcPs),
    FunReprSig (HsType GhcPs) ~ (),
    FunReprCtx (HsType GhcPs) ~ HsType GhcPs,
    FunReprArg (HsType GhcPs) ~ HsType GhcPs,
    FunReprArr (HsType GhcPs) ~ HsType GhcPs,
    FunReprRet (HsType GhcPs) ~ HsType GhcPs
  )

hasDocs :: ParsedFunRepr' ig ctx arg arr ret -> Bool
hasDocs = \case
  ParsedFunSig {next} -> hasDocs next
  ParsedFunForall {next} -> hasDocs next
  ParsedFunQuals {next} -> hasDocs next
  ParsedFunArg {doc, next} -> isJust doc || hasDocs next
  ParsedFunReturn {doc} -> isJust doc

{----- Rendering ParsedFunRepr -----}

-- | For implementing function-arrows and related configuration, we'll collect
-- all the components of the function type first, then render as a block instead
-- of rendering each part independently, which will let us track local state
-- within a function type.
--
-- This function should be passed the first function-related construct we find;
-- see FunRepr for more details.
p_hsFun :: forall a. (FunRepr a, FunReprHsType) => a -> R ()
p_hsFun = p_hsFunParsed @a . parseFunRepr . L (noAnn @SrcSpanAnnA)

p_hsFunParsed :: forall a. (FunRepr a, FunReprHsType) => ParsedFunRepr a -> R ()
p_hsFunParsed = runPrintHsFun . p_hsFunParsed' @a

p_hsFunParsed' :: forall a. (FunRepr a, FunReprHsType) => ParsedFunRepr a -> PrintHsFun ()
p_hsFunParsed' = \case
  ParsedFunSig {sig, next} -> do
    -- Should only happen at the very beginning, if at all
    p_parsedFunSig @a sig next
    setMultilineContext next
    p_hsFunParsed' @a next
  ParsedFunForall {tele, next} -> do
    p_parsedFunForall tele
    setMultilineContext next
    p_hsFunParsed' @a next
  ParsedFunQuals {ctxs, next} -> do
    p_parsedFunQuals @a ctxs
    setMultilineContext next
    p_hsFunParsed' @a next
  ParsedFunArg {span, arg, doc, arrow, next} -> do
    p_parsedFunArg @a span arg doc arrow
    case next of
      ParsedFunArg {} -> pure ()
      _ -> do
        -- Don't reset multiline context within args; all args should be on
        -- one line together, or all on separate lines
        setMultilineContext next
        -- Reset inArgList, needed if there are multiple arg lists in one
        -- function type (e.g. `a -> Show a => b -> c`)
        setInArgList False
    p_hsFunParsed' @a next
  ParsedFunReturn {ret, doc} -> do
    p_parsedFunReturn @a ret doc

p_parsedFunSig :: forall a. (FunRepr a) => FunReprSig a -> ParsedFunRepr a -> PrintHsFun ()
p_parsedFunSig sig fun = do
  isTrailing <- getIsTrailing
  if isTrailing (Isn't #argDelim)
    then liftR $ do
      renderFunReprSig @a sig
      if hasDocs fun then newline else breakpoint
    else do
      liftR breakpoint
      setLeadingDelim (token'dcolon >> space)

p_parsedFunForall :: (FunReprHsType) => LocatedA (HsForAllTelescope GhcPs) -> PrintHsFun ()
p_parsedFunForall x@(L _ tele) = do
  setLocated_ x
  applyLeadingDelim
  vis <-
    case tele of
      HsForAllInvis _ bndrs -> do
        liftR $ p_forallBndrsStart p_hsTyVarBndr bndrs
        pure ForAllInvis
      HsForAllVis _ bndrs -> do
        liftR $ p_forallBndrsStart p_hsTyVarBndr bndrs
        pure ForAllVis

  isTrailing <- getIsTrailing
  isMultiline <- getIsMultiline
  if isTrailing (Isn't #argDelim) || not isMultiline
    then do
      liftR $ p_forallBndrsEnd (Without #extraSpace) vis
      interArgBreak
    else do
      interArgBreak
      let extraSpace = if isMultiline then With #extraSpace else Without #extraSpace
      setLeadingDelim (p_forallBndrsEnd extraSpace vis)

p_parsedFunQuals :: forall a. (FunRepr a) => [LocatedA (LocatedC [LocatedA (FunReprCtx a)])] -> PrintHsFun ()
p_parsedFunQuals ctxs = do
  isTrailing <- getIsTrailing
  -- we only want to set located on the first context
  case ctxs of
    ctx : _ -> setLocated_ ctx
    _ -> pure ()

  forM_ ctxs $ \(L _ lctx) -> do
    applyLeadingDelim
    liftR $ located lctx (p_hsContext' (renderFunReprCtx @a))
    if isTrailing (Isn't #argDelim)
      then do
        liftR $ space >> token'darrow
        interArgBreak
      else do
        interArgBreak
        setLeadingDelim (token'darrow >> space)

p_parsedFunArg ::
  forall a.
  (FunRepr a) =>
  SrcSpanAnnA ->
  LocatedA (FunReprArg a) ->
  Maybe (LHsDoc GhcPs) ->
  HsArrowOf (LocatedA (FunReprArr a)) GhcPs ->
  PrintHsFun ()
p_parsedFunArg span item doc arrow = do
  isTrailing <- getIsTrailing

  -- We only want to set located on the first arg
  inArgList <- getInArgList
  when (not inArgList) $ do
    setLocated_ (L span item)
    setInArgList True

  let renderArrow = p_arrow (located' (renderFunReprArr @a)) arrow
  withHaddocks (Isn't #end) doc $ do
    withApplyLeadingDelim $ \applyLeadingDelim' ->
      liftR . located item $ \arg -> do
        traverse_ id applyLeadingDelim'
        renderFunReprArg @a arg
    if isTrailing (Is #argDelim)
      then do
        liftR $ space >> renderArrow
        interArgBreak
      else do
        interArgBreak
        setLeadingDelim (renderArrow >> space)

p_parsedFunReturn ::
  forall a.
  (FunRepr a) =>
  LocatedA (FunReprRet a) ->
  Maybe (LHsDoc GhcPs) ->
  PrintHsFun ()
p_parsedFunReturn item doc = do
  setLocated_ item
  withHaddocks (Is #end) doc $ do
    applyLeadingDelim
    liftR $ located item (renderFunReprRet @a)

{----- forall -----}

data ForAllVisibility = ForAllInvis | ForAllVis

-- | Render several @forall@-ed variables.
p_forallBndrs ::
  (HasLoc l) =>
  ForAllVisibility ->
  (a -> R ()) ->
  [GenLocated l a] ->
  R ()
p_forallBndrs vis p tyvars = do
  p_forallBndrsStart p tyvars
  p_forallBndrsEnd (Without #extraSpace) vis

p_forallBndrsStart :: (HasLoc l) => (a -> R ()) -> [GenLocated l a] -> R ()
p_forallBndrsStart _ [] = token'forall
p_forallBndrsStart p tyvars = do
  switchLayout (locA <$> tyvars) $ do
    token'forall
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars

p_forallBndrsEnd :: Choice "extraSpace" -> ForAllVisibility -> R ()
p_forallBndrsEnd extraSpace = \case
  ForAllInvis -> txt dot >> space
  ForAllVis -> space >> token'rarrow
  where
    dot = if Choice.isTrue extraSpace then " ." else "."

{----- HsTyVarBndr -----}

class IsTyVarBndrFlag flag where
  isInferred :: flag -> Bool
  p_tyVarBndrFlag :: flag -> R ()
  p_tyVarBndrFlag _ = pure ()

instance IsTyVarBndrFlag () where
  isInferred () = False

instance IsTyVarBndrFlag Specificity where
  isInferred = \case
    InferredSpec -> True
    SpecifiedSpec -> False

instance IsTyVarBndrFlag (HsBndrVis GhcPs) where
  isInferred _ = False
  p_tyVarBndrFlag = \case
    HsBndrRequired NoExtField -> pure ()
    HsBndrInvisible _ -> txt "@"

p_hsTyVarBndr ::
  (IsTyVarBndrFlag flag, FunReprHsType) =>
  HsTyVarBndr flag GhcPs -> R ()
p_hsTyVarBndr HsTvb {..} = do
  p_tyVarBndrFlag tvb_flag
  let wrap
        | isInferred tvb_flag = braces N
        | otherwise = case tvb_kind of
            HsBndrKind {} -> parens N
            HsBndrNoKind {} -> id
  wrap $ do
    case tvb_var of
      HsBndrVar _ x -> p_rdrName x
      HsBndrWildCard _ -> txt "_"
    case tvb_kind of
      HsBndrKind _ k -> inci $ do
        p_hsFunParsed @(HsType GhcPs) $
          ParsedFunSig
            { sig = (),
              next = parseFunRepr k
            }
      HsBndrNoKind _ -> pure ()

{----- Contexts -----}

p_hsContext' ::
  (Outputable (GenLocated (Anno a) a), HasLoc (Anno a)) =>
  (a -> R ()) ->
  [XRec GhcPs a] ->
  R ()
p_hsContext' f = \case
  [] -> txt "()"
  [x] -> located x f
  xs -> do
    shouldSort <- getPrinterOpt poSortConstraints
    let sort = if shouldSort then sortOn showOutputable else id
    parens N $ sep commaDel (sitcc . located' f) (sort xs)

{----- Helpers -----}

withHaddocks :: (MonadR m) => Choice "end" -> Maybe (LHsDoc GhcPs) -> m a -> m a
withHaddocks isEnd doc m = do
  isTrailing <- getIsTrailing
  isLeadingHaddock <-
    liftR $
      getPrinterOpt poHaddockLocSignature <&> \case
        HaddockLocSigAuto ->
          if isTrailing (Is #argDelim) then With #pipe else Without #pipe
        HaddockLocSigLeading ->
          With #pipe
        HaddockLocSigTrailing ->
          Without #pipe

  if Choice.isTrue isLeadingHaddock
    then do
      traverse (liftR . p_hsDoc Pipe (With #endNewline)) doc *> m
    else do
      let (pre, endNewline) =
            if Choice.isTrue isEnd
              then (when (isJust doc) (liftR newline), Without #endNewline)
              else (pure (), With #endNewline)
      m <* pre <* traverse (liftR . p_hsDoc Caret endNewline) doc

interArgBreak :: PrintHsFun ()
interArgBreak = do
  isMultiline <- getIsMultiline
  liftR $ if isMultiline then newline else breakpoint
