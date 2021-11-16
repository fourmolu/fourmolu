{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.CLI.TH where

import Control.Monad (zipWithM)
import Data.Functor.Identity (runIdentity)
import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Ormolu.CLI (toCLI, toCLIArgument)
import Ormolu.Config (PrinterOpts (..), defaultPrinterOpts)

poFieldNames :: Q (TExp [String])
poFieldNames = do
  ConstructorInfo {constructorVariant = RecordConstructor names} <- reifyConstructor 'PrinterOpts
  let names' = toCLI . nameBase <$> names
  [||names'||]

displayCustomPrinterOpts :: Q Exp
displayCustomPrinterOpts = do
  ConstructorInfo {constructorFields} <- reifyConstructor 'PrinterOpts

  (xs_, xs) <- unzip <$> mapM (const $ mkVars "po") constructorFields
  (ds_, ds) <- unzip <$> mapM (const $ mkVars "defaultPo") constructorFields

  comparisons <-
    zipWithM
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
        [|$(pure $ ListE comparisons)|]
    )

mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  x <- newName name
  return (varP x, varE x)
