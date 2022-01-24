{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.Config.TH
  ( allNothing,
    unpackFieldsWithSuffix,
  )
where

import Control.Monad ((>=>))
import Language.Haskell.TH
import Text.Printf (printf)

allNothing :: Name -> Q Exp
allNothing name = do
  ty <- reifyType name
  foldl appE (conE name) $
    replicate (getArity ty) [|Nothing|]

unpackFieldsWithSuffix :: Name -> String -> Q Pat
unpackFieldsWithSuffix name suffix = do
  typeForCon <-
    reify name >>= \case
      DataConI _ _ typeForCon -> return typeForCon
      info -> fail $ "allNothing requires the Name of a data constructor, got: " <> show info

  allConsInType <-
    getAllConstructors typeForCon
      >>= either (fail . printf "Unexpected parent of data constructor: %s" . show) return

  fields <-
    case filter (elem name . getConstructorNames) allConsInType of
      [con] | Just fields <- conFieldNames con -> return fields
      _ -> fail $ "Could not find unique record constructor in: " <> show allConsInType

  conP name $ map (varP . mkName . (<> suffix) . nameBase) fields
  where
    conFieldNames = \case
      NormalC {} -> Nothing
      RecC _ tys -> Just $ map fst3 tys
      InfixC {} -> Nothing
      ForallC {} -> Nothing
      GadtC {} -> Nothing
      RecGadtC _ tys _ -> Just $ map fst3 tys
    fst3 (x, _, _) = x

----------------------------------------------------------------------------
-- Helpers

{- FOURMOLU_DISABLE -}
{- https://github.com/fourmolu/fourmolu#limitations -}
getArity :: Type -> Int
getArity = \case
  ForallT _ _ ty -> getArity ty
  AppT (AppT ArrowT _) ty -> 1 + getArity ty
#if MIN_VERSION_template_haskell(2,17,0)
  AppT (AppT (AppT MulArrowT _) _) ty -> 1 + getArity ty
#endif
  _ -> 0
{- FOURMOLU_ENABLE -}

getAllConstructors :: Name -> Q (Either Info [Con])
getAllConstructors =
  reify >=> \case
    TyConI (DataD _ _ _ _ cons _) -> return $ Right cons
    info -> return $ Left info

-- Could return multiple names for GADTs like 'A, B :: Foo'
getConstructorNames :: Con -> [Name]
getConstructorNames = \case
  NormalC n _ -> [n]
  RecC n _ -> [n]
  InfixC _ n _ -> [n]
  ForallC _ _ c -> getConstructorNames c
  GadtC ns _ _ -> ns
  RecGadtC ns _ _ -> ns
