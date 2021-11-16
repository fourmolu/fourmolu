{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.CLI.TH
  ( poFieldNames,
    displayCustomPrinterOpts,
  )
where

import Data.Aeson (camelTo2)
import Data.Functor.Identity (runIdentity)
import "template-haskell" Language.Haskell.TH hiding (newName)
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax.Compat
import Ormolu.CLI
import Ormolu.Config (PrinterOpts (..), defaultPrinterOpts)

poFieldNames :: SpliceQ [String]
poFieldNames = liftSplice $ do
  ConstructorInfo {constructorVariant = RecordConstructor names} <- reifyConstructor 'PrinterOpts
  let namesStrs = toCLI . nameBase <$> names
  examineSplice [||namesStrs||]

displayCustomPrinterOpts :: Q Exp
displayCustomPrinterOpts = do
  ConstructorInfo {constructorFields} <- reifyConstructor 'PrinterOpts

  (xs_, xs) <- unzip <$> mapM (const $ mkVars "po") constructorFields
  (ds_, ds) <- unzip <$> mapM (const $ mkVars "defaultPo") constructorFields

  let comparisons =
        zipWith
          ( \x d ->
              [|
                if $x /= $d
                  then Just . toCLIArgument . runIdentity $ $x
                  else mempty
                |]
          )
          xs
          ds

  lamE
    [conP 'PrinterOpts xs_]
    ( letE
        [ valD
            (conP 'PrinterOpts ds_)
            (normalB [|defaultPrinterOpts|])
            []
        ]
        [|$(listE comparisons)|]
    )

mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  x <- newName name
  return (varP x, varE x)

toCLI :: String -> String
toCLI = camelTo2 '-' . drop 2
