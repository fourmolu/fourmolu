-- | This module contains the source of truth for configuration fields.
--
-- This module is broken up into two sections: the first is the list of
-- options available in Fourmolu configuration, and the second is the
-- definition of new Haskell types.
module FourmoluConfig.ConfigData where

{----- Options -----}

data Option = Option
  { -- | Name of key in config file + flag in CLI
    name :: String,
    -- | The Haskell type of the option
    type_ :: String,
    -- | Description of option in Haddocks + CLI
    description :: String,
    -- | Name of PrinterOpts field (or Nothing if config is not in PrinterOpts)
    fieldName :: Maybe String,
    -- | The option to use as the Fourmolu default
    default_ :: HaskellValue,
    -- | The option that mimics Ormolu's formatting
    ormolu :: HaskellValue,
    -- | The version the option was added in
    sinceVersion :: Maybe String,
    -- | Overriding CLI information
    cliOverrides :: CLIOverrides
  }

data CLIOverrides = CLIOverrides
  { -- | Text to show in the CLI help text; defaults to `description`
    cliHelp :: Maybe String,
    -- | Text to add to the CLI help text as the default; defaults to `default`
    cliDefault :: Maybe String,
    -- | Placeholder to use in CLI help text; by default, determined based on `type_`
    cliPlaceholder :: Maybe String
  }

emptyOverrides :: CLIOverrides
emptyOverrides = CLIOverrides Nothing Nothing Nothing

data HaskellValue
  = -- | Some arbitrary Haskell expression like "MyConstructor True 1".
    -- Should be renderable by the appropriate `FieldType`
    HsExpr String
  | HsInt Int
  | HsBool Bool
  | HsList [HaskellValue]
  deriving (Eq)

allOptions :: [Option]
allOptions =
  [ Option
      { name = "preset",
        type_ = "ConfigPreset",
        description = "Preset to use as the base configuration",
        fieldName = Nothing,
        default_ = HsExpr "FourmoluPreset",
        ormolu = HsExpr "FourmoluPreset",
        sinceVersion = Nothing,
        cliOverrides = emptyOverrides
      },
    Option
      { name = "indentation",
        type_ = "Int",
        description = "Number of spaces per indentation step",
        fieldName = Just "poIndentation",
        default_ = HsInt 4,
        ormolu = HsInt 2,
        sinceVersion = Just "0.1.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "column-limit",
        type_ = "ColumnLimit",
        description = "Max line length for automatic line breaking",
        fieldName = Just "poColumnLimit",
        default_ = HsExpr "NoLimit",
        ormolu = HsExpr "NoLimit",
        sinceVersion = Just "0.12.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "function-arrows",
        type_ = "FunctionArrowsStyle",
        description = "Styling of arrows in type signatures",
        fieldName = Just "poFunctionArrows",
        default_ = HsExpr "TrailingArrows",
        ormolu = HsExpr "TrailingArrows",
        sinceVersion = Just "0.8.2.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "comma-style",
        type_ = "CommaStyle",
        description = "How to place commas in multi-line lists, records, etc.",
        fieldName = Just "poCommaStyle",
        default_ = HsExpr "Leading",
        ormolu = HsExpr "Trailing",
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "import-export-style",
        type_ = "ImportExportStyle",
        description = "Styling of import/export lists",
        fieldName = Just "poImportExportStyle",
        default_ = HsExpr "ImportExportDiffFriendly",
        ormolu = HsExpr "ImportExportTrailing",
        sinceVersion = Just "0.8.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "indent-wheres",
        type_ = "Bool",
        description = "Whether to full-indent or half-indent 'where' bindings past the preceding body",
        fieldName = Just "poIndentWheres",
        default_ = HsBool False,
        ormolu = HsBool True,
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "record-brace-space",
        type_ = "Bool",
        description = "Whether to leave a space before an opening record brace",
        fieldName = Just "poRecordBraceSpace",
        default_ = HsBool False,
        ormolu = HsBool True,
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "newlines-between-decls",
        type_ = "Int",
        description = "Number of spaces between top-level declarations",
        fieldName = Just "poNewlinesBetweenDecls",
        default_ = HsInt 1,
        ormolu = HsInt 1,
        sinceVersion = Just "0.3.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "haddock-style",
        type_ = "HaddockPrintStyle",
        description = "How to print Haddock comments",
        fieldName = Just "poHaddockStyle",
        default_ = HsExpr "HaddockMultiLine",
        ormolu = HsExpr "HaddockSingleLine",
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "haddock-style-module",
        type_ = "HaddockPrintStyleModule",
        description = "How to print module docstring",
        fieldName = Just "poHaddockStyleModule",
        default_ = HsExpr "PrintStyleInherit",
        ormolu = HsExpr "PrintStyleInherit",
        sinceVersion = Just "0.10.0.0",
        cliOverrides = emptyOverrides {cliDefault = Just "same as 'haddock-style'"}
      },
    Option
      { name = "let-style",
        type_ = "LetStyle",
        description = "Styling of let blocks",
        fieldName = Just "poLetStyle",
        default_ = HsExpr "LetAuto",
        ormolu = HsExpr "LetInline",
        sinceVersion = Just "0.9.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "in-style",
        type_ = "InStyle",
        description = "How to align the 'in' keyword with respect to the 'let' keyword",
        fieldName = Just "poInStyle",
        default_ = HsExpr "InRightAlign",
        ormolu = HsExpr "InRightAlign",
        sinceVersion = Just "0.9.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "single-constraint-parens",
        type_ = "SingleConstraintParens",
        description = "Whether to put parentheses around a single constraint",
        fieldName = Just "poSingleConstraintParens",
        default_ = HsExpr "ConstraintAlways",
        ormolu = HsExpr "ConstraintAlways",
        sinceVersion = Just "0.12.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "unicode",
        type_ = "Unicode",
        description = "Output Unicode syntax",
        fieldName = Just "poUnicode",
        default_ = HsExpr "UnicodeNever",
        ormolu = HsExpr "UnicodeNever",
        sinceVersion = Just "0.9.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "respectful",
        type_ = "Bool",
        description = "Give the programmer more choice on where to insert blank lines",
        fieldName = Just "poRespectful",
        default_ = HsBool True,
        ormolu = HsBool False,
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "fixities",
        type_ = "[String]",
        description = "Fixity information for operators",
        fieldName = Nothing,
        default_ = HsList [],
        ormolu = HsList [],
        sinceVersion = Just "0.7.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "reexports",
        type_ = "[String]",
        description = "Module reexports Fourmolu should know about",
        fieldName = Nothing,
        default_ = HsList [],
        ormolu = HsList [],
        sinceVersion = Just "0.13.0.0",
        cliOverrides = emptyOverrides
      }
  ]

{----- Field types -----}

data FieldType
  = FieldTypeEnum
      { fieldTypeName :: String,
        -- | Maps constructor name to string representation
        enumOptions :: [(String, String)]
      }
  | FieldTypeADT
      { fieldTypeName :: String,
        adtConstructors :: [String],
        adtSchema :: ADTSchema,
        -- | Mapping from Haskell expression (in `HsExpr`) to string representation
        adtRender :: [(String, String)],
        -- | Implementation of `Aeson.parseJSON`
        adtParseJSON :: String,
        -- | Implementation of `String -> Either String a`
        adtParseFourmoluConfigType :: String
      }

-- | The definition of possible values in a data type.
data ADTSchema = ADTSchema
  { adtOptions :: [ADTSchemaOption],
    adtInputType :: ADTSchemaInputType
  }

data ADTSchemaOption
  = -- | A literal YAML value
    ADTOptionLiteral String
  | -- | A description of the option
    ADTOptionDescription String

data ADTSchemaInputType
  = ADTSchemaInputText [ADTSchemaInputParser]
  | ADTSchemaInputNumber
  | ADTSchemaInputCheckbox
  | ADTSchemaInputDropdown [ADTSchemaInputParser]

data ADTSchemaInputParser
  = ADTSchemaInputParserString
  | ADTSchemaInputParserNumber
  | ADTSchemaInputParserNull

allFieldTypes :: [FieldType]
allFieldTypes =
  [ FieldTypeEnum
      { fieldTypeName = "ConfigPreset",
        enumOptions =
          [ ("FourmoluPreset", "fourmolu"),
            ("OrmoluPreset", "ormolu")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "CommaStyle",
        enumOptions =
          [ ("Leading", "leading"),
            ("Trailing", "trailing")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "FunctionArrowsStyle",
        enumOptions =
          [ ("TrailingArrows", "trailing"),
            ("LeadingArrows", "leading"),
            ("LeadingArgsArrows", "leading-args")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "HaddockPrintStyle",
        enumOptions =
          [ ("HaddockSingleLine", "single-line"),
            ("HaddockMultiLine", "multi-line"),
            ("HaddockMultiLineCompact", "multi-line-compact")
          ]
      },
    FieldTypeADT
      { fieldTypeName = "HaddockPrintStyleModule",
        adtConstructors =
          [ "PrintStyleInherit",
            "PrintStyleOverride HaddockPrintStyle"
          ],
        adtSchema =
          ADTSchema
            { adtOptions =
                let printStyleOpts =
                      case filter ((== "HaddockPrintStyle") . fieldTypeName) allFieldTypes of
                        [FieldTypeEnum {enumOptions}] -> map snd enumOptions
                        _ -> error "Could not find HaddockPrintStyle option"
                 in map ADTOptionLiteral $ "null" : printStyleOpts,
              adtInputType =
                ADTSchemaInputDropdown
                  [ ADTSchemaInputParserNull,
                    ADTSchemaInputParserString
                  ]
            },
        adtRender = [("PrintStyleInherit", "null")],
        adtParseJSON =
          unlines
            [ "\\v -> case v of",
              "  Aeson.Null -> pure PrintStyleInherit",
              "  Aeson.String \"\" -> pure PrintStyleInherit",
              "  _ -> PrintStyleOverride <$> Aeson.parseJSON v"
            ],
        adtParseFourmoluConfigType =
          unlines
            [ "\\s -> case s of",
              "  \"\" -> pure PrintStyleInherit",
              "  _ -> PrintStyleOverride <$> parseFourmoluConfigType s"
            ]
      },
    FieldTypeEnum
      { fieldTypeName = "ImportExportStyle",
        enumOptions =
          [ ("ImportExportLeading", "leading"),
            ("ImportExportTrailing", "trailing"),
            ("ImportExportDiffFriendly", "diff-friendly")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "LetStyle",
        enumOptions =
          [ ("LetAuto", "auto"),
            ("LetInline", "inline"),
            ("LetNewline", "newline"),
            ("LetMixed", "mixed")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "InStyle",
        enumOptions =
          [ ("InLeftAlign", "left-align"),
            ("InRightAlign", "right-align"),
            ("InNoSpace", "no-space")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "Unicode",
        enumOptions =
          [ ("UnicodeDetect", "detect"),
            ("UnicodeAlways", "always"),
            ("UnicodeNever", "never")
          ]
      },
    FieldTypeEnum
      { fieldTypeName = "SingleConstraintParens",
        enumOptions =
          [ ("ConstraintAuto", "auto"),
            ("ConstraintAlways", "always"),
            ("ConstraintNever", "never")
          ]
      },
    FieldTypeADT
      { fieldTypeName = "ColumnLimit",
        adtConstructors =
          [ "NoLimit",
            "ColumnLimit Int"
          ],
        adtSchema =
          ADTSchema
            { adtOptions =
                [ ADTOptionLiteral "none",
                  ADTOptionDescription "Any non-negative integer"
                ],
              adtInputType =
                ADTSchemaInputText
                  [ ADTSchemaInputParserNumber,
                    ADTSchemaInputParserString
                  ]
            },
        adtRender = [("NoLimit", "none")],
        adtParseJSON =
          unlines
            [ "\\case",
              "   Aeson.String \"none\" ->",
              "     pure NoLimit",
              "   Aeson.Number x",
              "     | Right x' <- (floatingOrInteger x :: Either Double Int) ->",
              "         pure $ ColumnLimit x'",
              "   s ->",
              "     fail . unlines $",
              "       [ \"unknown value: \" <> show s,",
              "         \"Valid values are: \\\"none\\\", or an integer\"",
              "       ]"
            ],
        adtParseFourmoluConfigType =
          unlines
            [ "\\s ->",
              "  case s of",
              "    \"none\" -> Right NoLimit",
              "    _",
              "      | Just someInt <- readMaybe s ->",
              "          Right . ColumnLimit $ someInt",
              "    _ ->",
              "      Left . unlines $",
              "        [ \"unknown value: \" <> show s,",
              "          \"Valid values are: \\\"none\\\", or an integer\"",
              "        ]"
            ]
      }
  ]
