{-# LANGUAGE LambdaCase #-}

module FourmoluConfig.GenerateUtils where

import Control.Monad ((>=>))
import Data.List (intercalate, isSuffixOf, stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import FourmoluConfig.ConfigData

{----- Helpers -----}

fieldTypesMap :: Map String FieldType
fieldTypesMap = Map.fromList [(fieldTypeName fieldType, fieldType) | fieldType <- allFieldTypes]

getFieldType :: String -> FieldType
getFieldType name =
  case Map.lookup name fieldTypesMap of
    Just ty -> ty
    Nothing ->
      error . unwords $
        [ "Could not find field type '" <> name <> "'.",
          "Did you add it to 'allFieldTypes' in ConfigData.hs?"
        ]

getOptionSchema :: Option -> ADTSchema
getOptionSchema Option {type_ = ty} =
  case ty of
    "Int" ->
      ADTSchema
        { adtOptions = [ADTOptionDescription "Any integer"],
          adtInputType = ADTSchemaInputNumber
        }
    "Bool" ->
      ADTSchema
        { adtOptions = [ADTOptionDescription "Any boolean"],
          adtInputType = ADTSchemaInputCheckbox
        }
    "String" ->
      ADTSchema
        { adtOptions = [ADTOptionDescription "Any string"],
          adtInputType = ADTSchemaInputText [ADTSchemaInputParserString]
        }
    _ ->
      case getFieldType ty of
        FieldTypeEnum {enumOptions} ->
          ADTSchema
            { adtOptions = map (ADTOptionLiteral . snd) enumOptions,
              adtInputType = ADTSchemaInputDropdown [ADTSchemaInputParserString]
            }
        FieldTypeADT {adtSchema} -> adtSchema

-- | Render a HaskellValue for Haskell.
renderHs :: HaskellValue -> String
renderHs = \case
  HsExpr v -> v
  HsInt v -> show v
  HsBool v -> show v
  HsList vs -> "[" <> intercalate ", " (map renderHs vs) <> "]"

-- | Render a HaskellValue for YAML.
hs2yaml :: String -> HaskellValue -> String
hs2yaml hsType = \case
  HsExpr v ->
    fromMaybe (error $ "Could not render " <> hsType <> " value: " <> v) $
      case getFieldType hsType of
        FieldTypeEnum {enumOptions} -> v `lookup` enumOptions
        FieldTypeADT {adtRender} -> v `lookup` adtRender
  HsInt v -> show v
  HsBool v -> if v then "true" else "false"
  HsList vs ->
    let hsType' =
          case (stripPrefix "[" >=> stripSuffix "]") hsType of
            Just s -> s
            Nothing -> error $ "Not a list type: " <> hsType
     in "[" <> intercalate ", " (map (hs2yaml hsType') vs) <> "]"

{----- Utilities -----}

-- | Like 'unlines', except without a trailing newline.
unlines_ :: [String] -> String
unlines_ = intercalate "\n"

indent :: String -> String
indent = indent' 1

indent' :: Int -> String -> String
indent' n = unlines_ . map (replicate (n * 2) ' ' <>) . lines

quote :: String -> String
quote s = "\"" <> s <> "\""

renderList :: [String] -> String
renderList = \case
  [] -> ""
  [s] -> s
  [s1, s2] -> s1 <> " or " <> s2
  ss -> intercalate ", " (init ss) <> ", or " <> last ss

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix s =
  if suffix `isSuffixOf` s
    then Just $ take (length s - length suffix) s
    else Nothing

withFirst :: [a] -> [(Bool, a)]
withFirst = zip (True : repeat False)
