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
    -- | Name of PrinterOpts field (or Nothing if config is not in PrinterOpts)
    fieldName :: Maybe String,
    -- | Description of option in Haddocks + CLI
    description :: String,
    -- | The Haskell type of the option
    type_ :: String,
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
      { name = "indentation",
        fieldName = Just "poIndentation",
        description = "Number of spaces per indentation step",
        type_ = "Int",
        default_ = HsInt 4,
        ormolu = HsInt 2,
        sinceVersion = Just "0.1.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "column-limit",
        fieldName = Just "poColumnLimit",
        description = "Max line length for automatic line breaking",
        type_ = "ColumnLimit",
        default_ = HsExpr "NoLimit",
        ormolu = HsExpr "NoLimit",
        sinceVersion = Just "0.12.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "function-arrows",
        fieldName = Just "poFunctionArrows",
        description = "Styling of arrows in type signatures",
        type_ = "FunctionArrowsStyle",
        default_ = HsExpr "TrailingArrows",
        ormolu = HsExpr "TrailingArrows",
        sinceVersion = Just "0.8.2.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "comma-style",
        fieldName = Just "poCommaStyle",
        description = "How to place commas in multi-line lists, records, etc.",
        type_ = "CommaStyle",
        default_ = HsExpr "Leading",
        ormolu = HsExpr "Trailing",
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "import-export-style",
        fieldName = Just "poImportExportStyle",
        description = "Styling of import/export lists",
        type_ = "ImportExportStyle",
        default_ = HsExpr "ImportExportDiffFriendly",
        ormolu = HsExpr "ImportExportTrailing",
        sinceVersion = Just "0.8.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "indent-wheres",
        fieldName = Just "poIndentWheres",
        description = "Whether to full-indent or half-indent 'where' bindings past the preceding body",
        type_ = "Bool",
        default_ = HsBool False,
        ormolu = HsBool True,
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "record-brace-space",
        fieldName = Just "poRecordBraceSpace",
        description = "Whether to leave a space before an opening record brace",
        type_ = "Bool",
        default_ = HsBool False,
        ormolu = HsBool True,
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "newlines-between-decls",
        fieldName = Just "poNewlinesBetweenDecls",
        description = "Number of spaces between top-level declarations",
        type_ = "Int",
        default_ = HsInt 1,
        ormolu = HsInt 1,
        sinceVersion = Just "0.3.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "haddock-style",
        fieldName = Just "poHaddockStyle",
        description = "How to print Haddock comments",
        type_ = "HaddockPrintStyle",
        default_ = HsExpr "HaddockMultiLine",
        ormolu = HsExpr "HaddockSingleLine",
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "haddock-style-module",
        fieldName = Just "poHaddockStyleModule",
        description = "How to print module docstring",
        type_ = "HaddockPrintStyleModule",
        default_ = HsExpr "PrintStyleInherit",
        ormolu = HsExpr "PrintStyleInherit",
        sinceVersion = Just "0.10.0.0",
        cliOverrides = emptyOverrides {cliDefault = Just "same as 'haddock-style'"}
      },
    Option
      { name = "let-style",
        fieldName = Just "poLetStyle",
        description = "Styling of let blocks",
        type_ = "LetStyle",
        default_ = HsExpr "LetAuto",
        ormolu = HsExpr "LetInline",
        sinceVersion = Just "0.9.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "in-style",
        fieldName = Just "poInStyle",
        description = "How to align the 'in' keyword with respect to the 'let' keyword",
        type_ = "InStyle",
        default_ = HsExpr "InRightAlign",
        ormolu = HsExpr "InRightAlign",
        sinceVersion = Just "0.9.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "single-constraint-parens",
        fieldName = Just "poSingleConstraintParens",
        description = "Whether to put parentheses around a single constraint",
        type_ = "SingleConstraintParens",
        default_ = HsExpr "ConstraintAlways",
        ormolu = HsExpr "ConstraintAlways",
        sinceVersion = Just "0.12.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "single-deriving-parens",
        fieldName = Just "poSingleDerivingParens",
        description = "Whether to put parentheses around a single deriving class",
        type_ = "SingleDerivingParens",
        default_ = HsExpr "DerivingAlways",
        ormolu = HsExpr "DerivingAlways",
        sinceVersion = Just "0.15.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "unicode",
        fieldName = Just "poUnicode",
        description = "Output Unicode syntax",
        type_ = "Unicode",
        default_ = HsExpr "UnicodeNever",
        ormolu = HsExpr "UnicodeNever",
        sinceVersion = Just "0.9.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "respectful",
        fieldName = Just "poRespectful",
        description = "Give the programmer more choice on where to insert blank lines",
        type_ = "Bool",
        default_ = HsBool True,
        ormolu = HsBool False,
        sinceVersion = Just "0.2.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "fixities",
        fieldName = Nothing,
        description = "Fixity information for operators",
        type_ = "[String]",
        default_ = HsList [],
        ormolu = HsList [],
        sinceVersion = Just "0.7.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "reexports",
        fieldName = Nothing,
        description = "Module reexports Fourmolu should know about",
        type_ = "[String]",
        default_ = HsList [],
        ormolu = HsList [],
        sinceVersion = Just "0.13.0.0",
        cliOverrides = emptyOverrides
      },
    Option
      { name = "import-grouping",
        fieldName = Just "poImportGrouping",
        description = "Rules for grouping import declarations",
        type_ = "ImportGrouping",
        default_ = HsExpr "ImportGroupLegacy",
        ormolu = HsExpr "ImportGroupLegacy",
        sinceVersion = Nothing,
        cliOverrides = emptyOverrides
      },
    Option
      { name = "local-modules",
        fieldName = Nothing,
        description = "Modules defined by the current Cabal package for import grouping",
        type_ = "[String]",
        default_ = HsList [],
        ormolu = HsList [],
        sinceVersion = Nothing,
        cliOverrides = emptyOverrides
      },
    Option
      { name = "sort-constraints",
        fieldName = Just "poSortConstraints",
        description = "Whether to sort constraints",
        type_ = "Bool",
        default_ = HsBool False,
        ormolu = HsBool False,
        sinceVersion = Nothing,
        cliOverrides = emptyOverrides
      },
    Option
      { name = "sort-derived-classes",
        fieldName = Just "poSortDerivedClasses",
        description = "Whether to sort derived classes",
        type_ = "Bool",
        default_ = HsBool False,
        ormolu = HsBool False,
        sinceVersion = Nothing,
        cliOverrides = emptyOverrides
      },
    Option
      { name = "sort-deriving-clauses",
        fieldName = Just "poSortDerivingClauses",
        description = "Whether to sort deriving clauses",
        type_ = "Bool",
        default_ = HsBool False,
        ormolu = HsBool False,
        sinceVersion = Nothing,
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
        adtParsePrinterOptType :: String
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
        adtParsePrinterOptType =
          unlines
            [ "\\s -> case s of",
              "  \"\" -> pure PrintStyleInherit",
              "  _ -> PrintStyleOverride <$> parsePrinterOptType s"
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
        adtParsePrinterOptType =
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
      },
    FieldTypeEnum
      { fieldTypeName = "SingleDerivingParens",
        enumOptions =
          [ ("DerivingAuto", "auto"),
            ("DerivingAlways", "always"),
            ("DerivingNever", "never")
          ]
      },
    FieldTypeADT
      { fieldTypeName = "ImportGrouping",
        adtConstructors =
          [ "ImportGroupLegacy",
            "ImportGroupPreserve",
            "ImportGroupSingle",
            "ImportGroupByScope",
            "ImportGroupByQualified",
            "ImportGroupByScopeThenQualified",
            "ImportGroupCustom (NonEmpty CT.ImportGroup)"
          ],
        adtSchema =
          ADTSchema
            { adtOptions =
                [ ADTOptionLiteral "legacy",
                  ADTOptionLiteral "preserve",
                  ADTOptionLiteral "single",
                  ADTOptionLiteral "by-qualified",
                  ADTOptionLiteral "by-scope",
                  ADTOptionLiteral "by-scope-then-qualified"
                  -- The custom import grouping rules can't be edited on the site yet.
                ],
              adtInputType =
                ADTSchemaInputText
                  [ ADTSchemaInputParserString
                  ]
            },
        adtRender =
          [ ("ImportGroupLegacy", "legacy"),
            ("ImportGroupPreserve", "preserve"),
            ("ImportGroupSingle", "single"),
            ("ImportGroupByQualified", "by-qualified"),
            ("ImportGroupByScope", "by-scope"),
            ("ImportGroupByScopeThenQualified", "by-scope-then-qualified")
          ],
        adtParseJSON =
          unlines
            [ "\\case",
              "  Aeson.String \"legacy\" -> pure ImportGroupLegacy",
              "  Aeson.String \"preserve\" -> pure ImportGroupPreserve",
              "  Aeson.String \"single\" -> pure ImportGroupSingle",
              "  Aeson.String \"by-qualified\" -> pure ImportGroupByQualified",
              "  Aeson.String \"by-scope\" -> pure ImportGroupByScope",
              "  Aeson.String \"by-scope-then-qualified\" -> pure ImportGroupByScopeThenQualified",
              "  arr@(Aeson.Array _) -> ImportGroupCustom <$> Aeson.parseJSON arr",
              "  other ->",
              "    fail . unlines $",
              "      [ \"unknown strategy value: \" <> show other,",
              "        \"Valid values are: \\\"legacy\\\", \\\"preserve\\\", \\\"single\\\", \\\"by-qualified\\\", \\\"by-scope\\\", \\\"by-scope-then-qualified\\\" or a valid YAML configuration for import groups\"",
              "      ]"
            ],
        adtParsePrinterOptType =
          unlines
            [ "\\case",
              "  \"legacy\" -> Right ImportGroupLegacy",
              "  \"preserve\" -> Right ImportGroupPreserve",
              "  \"single\" -> Right ImportGroupSingle",
              "  \"by-qualified\" -> Right ImportGroupByQualified",
              "  \"by-scope\" -> Right ImportGroupByScope",
              "  \"by-scope-then-qualified\" -> Right ImportGroupByScopeThenQualified",
              "  s ->",
              "    Left . unlines $",
              "      [ \"unknown value: \" <> show s",
              "      , \"Valid values are: \\\"legacy\\\", \\\"preserve\\\", \\\"single\\\", \\\"by-qualified\\\", \\\"by-scope\\\", \\\"by-scope-then-qualified\\\" or a valid YAML configuration for import groups (see fourmolu.yaml)\"",
              "      ]"
            ]
      }
  ]
