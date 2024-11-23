{-# LANGUAGE LambdaCase #-}

module Ormolu.Utils.Fixity
  ( parseFixityDeclarationStr,
    parseModuleReexportDeclarationStr,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName (PackageName)
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Text.Megaparsec (errorBundlePretty)

-- | A wrapper around 'parseFixityDeclaration' for parsing individual fixity
-- definitions.
parseFixityDeclarationStr ::
  -- | Input to parse
  String ->
  -- | Parse result
  Either String [(OpName, FixityInfo)]
parseFixityDeclarationStr =
  first errorBundlePretty . parseFixityDeclaration . T.pack

-- | A wrapper around 'parseModuleReexportDeclaration' for parsing
-- a individual module reexport.
parseModuleReexportDeclarationStr ::
  -- | Input to parse
  String ->
  -- | Parse result
  Either String (ModuleName, NonEmpty (Maybe PackageName, ModuleName))
parseModuleReexportDeclarationStr =
  first errorBundlePretty . parseModuleReexportDeclaration . T.pack
