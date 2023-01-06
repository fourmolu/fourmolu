let Enum =
      < Leading
      | Trailing
      | TrailingArrows
      | LeadingArrows
      | LeadingArgsArrows
      | HaddockSingleLine
      | HaddockMultiLine
      | HaddockMultiLineCompact
      | PrintStyleInherit
      | ImportExportLeading
      | ImportExportTrailing
      | ImportExportDiffFriendly
      | LetAuto
      | LetInline
      | LetNewline
      | LetMixed
      | InLeftAlign
      | InRightAlign
      | UnicodeDetect
      | UnicodeAlways
      | UnicodeNever
      | False
      | True
      >

let Value = < Natural : Natural | Text : Text | Enum : Enum >

let showEnum =
      \(x : Enum) ->
        merge
          { Leading = "Leading"
          , Trailing = "Trailing"
          , TrailingArrows = "TrailingArrows"
          , LeadingArrows = "LeadingArrows"
          , LeadingArgsArrows = "LeadingArgsArrows"
          , HaddockSingleLine = "HaddockSingleLine"
          , HaddockMultiLine = "HaddockMultiLine"
          , HaddockMultiLineCompact = "HaddockMultiLineCompact"
          , PrintStyleInherit = "PrintStyleInherit"
          , ImportExportLeading = "ImportExportLeading"
          , ImportExportTrailing = "ImportExportTrailing"
          , ImportExportDiffFriendly = "ImportExportDiffFriendly"
          , LetAuto = "LetAuto"
          , LetInline = "LetInline"
          , LetNewline = "LetNewline"
          , LetMixed = "LetMixed"
          , InLeftAlign = "InLeftAlign"
          , InRightAlign = "InRightAlign"
          , UnicodeDetect = "UnicodeDetect"
          , UnicodeAlways = "UnicodeAlways"
          , UnicodeNever = "UnicodeNever"
          , False = "False"
          , True = "True"
          }
          x

let showEnumPretty =
      \(x : Enum) ->
        merge
          { Leading = "leading"
          , Trailing = "trailing"
          , TrailingArrows = "trailing"
          , LeadingArrows = "leading"
          , LeadingArgsArrows = "leading-args"
          , HaddockSingleLine = "single-line"
          , HaddockMultiLine = "multi-line"
          , HaddockMultiLineCompact = "multi-line-compact"
          , PrintStyleInherit = "null"
          , ImportExportLeading = "leading"
          , ImportExportTrailing = "trailing"
          , ImportExportDiffFriendly = "diff-friendly"
          , LetAuto = "auto"
          , LetInline = "inline"
          , LetNewline = "newline"
          , LetMixed = "mixed"
          , InLeftAlign = "left-align"
          , InRightAlign = "right-align"
          , UnicodeDetect = "detect"
          , UnicodeAlways = "always"
          , UnicodeNever = "never"
          , False = "false"
          , True = "true"
          }
          x

let showValue
    : Value -> Text
    = \(x : Value) ->
        merge
          { Natural = Natural/show, Text = \(x : Text) -> x, Enum = showEnum }
          x

let showValuePretty
    : Value -> Text
    = \(x : Value) ->
        merge
          { Natural = Natural/show
          , Text = \(x : Text) -> x
          , Enum = showEnumPretty
          }
          x

let EnumType = { name : Text, constructors : List Enum }

let CommaStyle
    : EnumType
    = { name = "CommaStyle", constructors = [ Enum.Leading, Enum.Trailing ] }

let FunctionArrowsStyle
    : EnumType
    = { name = "FunctionArrowsStyle"
      , constructors =
        [ Enum.TrailingArrows, Enum.LeadingArrows, Enum.LeadingArgsArrows ]
      }

let HaddockPrintStyle
    : EnumType
    = { name = "HaddockPrintStyle"
      , constructors =
        [ Enum.HaddockSingleLine
        , Enum.HaddockMultiLine
        , Enum.HaddockMultiLineCompact
        ]
      }

let ImportExportStyle
    : EnumType
    = { name = "ImportExportStyle"
      , constructors =
        [ Enum.ImportExportLeading
        , Enum.ImportExportTrailing
        , Enum.ImportExportDiffFriendly
        ]
      }

let LetStyle
    : EnumType
    = { name = "LetStyle"
      , constructors =
        [ Enum.LetAuto, Enum.LetInline, Enum.LetNewline, Enum.LetMixed ]
      }

let InStyle
    : EnumType
    = { name = "InStyle"
      , constructors = [ Enum.InLeftAlign, Enum.InRightAlign ]
      }

let Unicode
    : EnumType
    = { name = "Unicode"
      , constructors =
        [ Enum.UnicodeDetect, Enum.UnicodeAlways, Enum.UnicodeNever ]
      }

let Boolean
    : EnumType
    = { name = "Bool", constructors = [ Enum.False, Enum.True ] }

let ADT =
      { name : Text
      , constructors : List Text
      , parseJSON : Text
      , parsePrinterOptType : Text
      , cli : Text
      }

let HaddockPrintStyleModule
    : ADT
    = { name = "HaddockPrintStyleModule"
      , constructors =
        [ "PrintStyleInherit", "PrintStyleOverride HaddockPrintStyle" ]
      , parseJSON =
          ''
          \v -> case v of
            Aeson.Null -> pure PrintStyleInherit
            Aeson.String "" -> pure PrintStyleInherit
            _ -> PrintStyleOverride <$> Aeson.parseJSON v''
      , parsePrinterOptType =
          ''
          \s -> case s of
            "" -> pure PrintStyleInherit
            _ -> PrintStyleOverride <$> parsePrinterOptType s''
      , cli = "How to print module docstring (default: same as 'haddock-style')"
      }

let FieldType = < Enum : EnumType | ADT : ADT >

let typeName =
      \(t : FieldType) ->
        merge { Enum = \(x : EnumType) -> x.name, ADT = \(x : ADT) -> x.name } t

let OptionType = < Bool | Natural | Text | Enum : EnumType | ADT : ADT >

let showType =
      \(t : OptionType) ->
        merge
          { Bool = "Bool"
          , Natural = "Int"
          , Text = "Text"
          , ADT = \(x : ADT) -> x.name
          , Enum = \(x : EnumType) -> x.name
          }
          t

let showPlaceholder =
      \(t : OptionType) ->
        merge
          { Bool = "BOOL"
          , Natural = "INT"
          , Text = "TEXT"
          , Enum = \(ft : EnumType) -> "OPTION"
          , ADT = \(x : ADT) -> "OPTION"
          }
          t

let fieldTypes
    : List FieldType
    = [ FieldType.Enum CommaStyle
      , FieldType.Enum FunctionArrowsStyle
      , FieldType.Enum HaddockPrintStyle
      , FieldType.ADT HaddockPrintStyleModule
      , FieldType.Enum ImportExportStyle
      , FieldType.Enum LetStyle
      , FieldType.Enum InStyle
      , FieldType.Enum Unicode
      ]

let Option =
      { name : Text
      , fieldName : Text
      , description : Text
      , type : OptionType
      , default : Value
      , ormolu : Value
      }

let options
    : List Option
    = [ { name = "indentation"
        , fieldName = "poIndentation"
        , description = "Number of spaces per indentation step"
        , type = OptionType.Natural
        , default = Value.Natural 4
        , ormolu = Value.Natural 2
        }
      , { name = "function-arrows"
        , fieldName = "poFunctionArrows"
        , description = "Styling of arrows in type signatures"
        , type = OptionType.Enum FunctionArrowsStyle
        , default = Value.Enum Enum.TrailingArrows
        , ormolu = Value.Enum Enum.TrailingArrows
        }
      , { name = "comma-style"
        , fieldName = "poCommaStyle"
        , description = "How to place commas in multi-line lists, records, etc."
        , type = OptionType.Enum CommaStyle
        , default = Value.Enum Enum.Leading
        , ormolu = Value.Enum Enum.Trailing
        }
      , { name = "import-export-style"
        , fieldName = "poImportExportStyle"
        , description = "Styling of import/export lists"
        , type = OptionType.Enum ImportExportStyle
        , default = Value.Enum Enum.ImportExportDiffFriendly
        , ormolu = Value.Enum Enum.ImportExportTrailing
        }
      , { name = "indent-wheres"
        , fieldName = "poIndentWheres"
        , description =
            "Whether to full-indent or half-indent 'where' bindings past the preceding body"
        , type = OptionType.Bool
        , default = Value.Enum Enum.False
        , ormolu = Value.Enum Enum.True
        }
      , { name = "record-brace-space"
        , fieldName = "poRecordBraceSpace"
        , description =
            "Whether to leave a space before an opening record brace"
        , type = OptionType.Bool
        , default = Value.Enum Enum.False
        , ormolu = Value.Enum Enum.True
        }
      , { name = "newlines-between-decls"
        , fieldName = "poNewlinesBetweenDecls"
        , description = "Number of spaces between top-level declarations"
        , type = OptionType.Natural
        , default = Value.Natural 1
        , ormolu = Value.Natural 1
        }
      , { name = "haddock-style"
        , fieldName = "poHaddockStyle"
        , description = "How to print Haddock comments"
        , type = OptionType.Enum HaddockPrintStyle
        , default = Value.Enum Enum.HaddockMultiLine
        , ormolu = Value.Enum Enum.HaddockSingleLine
        }
      , { name = "haddock-style-module"
        , fieldName = "poHaddockStyleModule"
        , description = "How to print module docstring"
        , type = OptionType.ADT HaddockPrintStyleModule
        , default = Value.Enum Enum.PrintStyleInherit
        , ormolu = Value.Enum Enum.PrintStyleInherit
        }
      , { name = "let-style"
        , fieldName = "poLetStyle"
        , description = "Styling of let blocks"
        , type = OptionType.Enum LetStyle
        , default = Value.Enum Enum.LetAuto
        , ormolu = Value.Enum Enum.LetInline
        }
      , { name = "in-style"
        , fieldName = "poInStyle"
        , description =
            "How to align the 'in' keyword with respect to the 'let' keyword"
        , type = OptionType.Enum InStyle
        , default = Value.Enum Enum.InRightAlign
        , ormolu = Value.Enum Enum.InRightAlign
        }
      , { name = "unicode"
        , fieldName = "poUnicode"
        , description = "Output Unicode syntax"
        , type = OptionType.Enum Unicode
        , default = Value.Enum Enum.UnicodeNever
        , ormolu = Value.Enum Enum.UnicodeNever
        }
      , { name = "respectful"
        , fieldName = "poRespectful"
        , description =
            "Give the programmer more choice on where to insert blank lines"
        , type = OptionType.Bool
        , default = Value.Enum Enum.True
        , ormolu = Value.Enum Enum.False
        }
      ]

in  { showPlaceholder
    , showType
    , showEnum
    , showEnumPretty
    , showValue
    , showValuePretty
    , Option
    , Enum
    , ADT
    , Boolean
    , EnumType
    , FieldType
    , typeName
    , options
    , fieldTypes
    }
