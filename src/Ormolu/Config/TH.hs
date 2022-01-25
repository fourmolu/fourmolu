{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.Config.TH
  ( allNothing,
    unpackFieldsWithSuffix,

    -- * BijectiveMap
    BijectiveMap,
    mkBijectiveMap,
    parseTextWith,
    showTextWith,
    showAllValues,
  )
where

import Control.Monad (forM, (>=>))
import Data.List (intercalate, nub)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
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

data BijectiveMap a = BijectiveMap
  { parseTextWith :: String -> Either String a,
    showTextWith :: a -> String,
    getAllOptions :: [String]
  }

showAllValues :: BijectiveMap a -> String
showAllValues = uncommas . map show . getAllOptions

-- | Generate a `BijectiveMap a` value with the given map.
--
-- Checks the following:
--   * all Names in given list refer to a constructor of type `a`
--   * all Names in given list refer to a 0-arity constructor
--   * all constructors in type `a` are accounted for.
mkBijectiveMap :: [(Name, String)] -> Q Exp
mkBijectiveMap mapping = do
  let (conNames, allOptions) = unzip mapping

  -- check all names refer to constructors
  (conTypes, conParents) <-
    fmap unzip . forM conNames $ \name ->
      reify name >>= \case
        DataConI _ ty parent -> pure (ty, parent)
        info ->
          fail $
            printf
              "mkBijectiveMap requires all Names refer to data constructors, got %s: %s"
              (show name)
              (show info)

  -- check that all constructors are in same type
  parent <-
    case nub conParents of
      [parent] -> return parent
      parents -> fail $ "mkBijectiveMap requires all Names refer to data constructors in the same type, got: " <> show parents

  -- check that all constructors are 0-arity
  case filter ((/= 0) . getArity) conTypes of
    [] -> return ()
    _ -> fail "mkBijectiveMap requires all constructors have 0-arity"

  -- check that all constructors are represented
  allConsInType <-
    getAllConstructors parent
      >>= either (fail . printf "Unexpected parent of data constructors: %s" . show) return
  case filter (`notElem` conNames) (concatMap getConstructorNames allConsInType) of
    [] -> return ()
    missing -> fail $ "Missing constructors: " ++ show missing

  unknown <- newName "unknown"
  let parser =
        lamCaseE . concat $
          [ flip map mapping $ \(name, option) ->
              match
                (litP $ stringL option)
                (normalB [|Right $(conE name)|])
                [],
            [ match
                (varP unknown)
                ( normalB
                    [|
                      Left . unlines $
                        [ "unknown value: " <> show $(varE unknown),
                          "Valid values are: " <> $(lift $ uncommas $ map show allOptions)
                        ]
                      |]
                )
                []
            ]
          ]
      shower =
        lamCaseE $
          flip map mapping $ \(name, option) ->
            match (conP name []) (normalB $ lift option) []

  [|
    BijectiveMap
      { parseTextWith = $parser,
        showTextWith = $shower,
        getAllOptions = $(lift allOptions)
      }
    |]

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

uncommas :: [String] -> String
uncommas [] = ""
uncommas [s] = s
uncommas [s0, s1] = s0 <> " or " <> s1
uncommas ss =
  let pre = init ss
      end = last ss
   in intercalate ", " pre <> "or " <> end
