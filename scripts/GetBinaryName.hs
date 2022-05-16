#!/usr/bin/env stack
{- stack script --resolver=lts --package Cabal -}

import Data.Char (toLower)
import Data.List (intercalate)
import Distribution.Package (packageName, packageVersion, unPackageName)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.System (buildArch, buildOS)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (versionNumbers)

main :: IO ()
main = do
  packageDesc <- readGenericPackageDescription Verbosity.silent "fourmolu.cabal"
  let name = unPackageName . packageName $ packageDesc
  let version = intercalate "." . map show . versionNumbers . packageVersion $ packageDesc
  let os = map toLower . show $ buildOS
  let arch = map toLower . show $ buildArch
  putStrLn $ intercalate "-" [name, version, os, arch]
