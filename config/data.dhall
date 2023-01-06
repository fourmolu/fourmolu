let Prelude = ./Prelude.dhall

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
      >

let Value = < Bool : Bool | Natural : Natural | Text : Text | Enum : Enum >

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
          }
          x

let showValue
    : Value -> Text
    = \(v : Value) ->
        merge
          { Bool = \(x : Bool) -> if x then "True" else "False"
          , Natural = Natural/show
          , Text = \(x : Text) -> x
          , Enum = showEnum
          }
          v

let showValuePretty
    : Value -> Text
    = \(v : Value) ->
        merge
          { Bool = \(x : Bool) -> if x then "true" else "false"
          , Natural = Natural/show
          , Text = \(x : Text) -> x
          , Enum = showEnumPretty
          }
          v

let CLI =
      { help : Optional Text
      , default : Optional Text
      , placeholder : Optional Text
      }

let CLITotal = { help : Text, default : Text, placeholder : Text }

let CommaStyle =
      { name = "CommaStyle", constructors = [ Enum.Leading, Enum.Trailing ] }

let FunctionArrowsStyle =
      { name = "FunctionArrowsStyle"
      , constructors =
        [ Enum.TrailingArrows, Enum.LeadingArrows, Enum.LeadingArgsArrows ]
      }

let HaddockPrintStyle =
      { name = "HaddockPrintStyle"
      , constructors =
        [ Enum.HaddockSingleLine
        , Enum.HaddockMultiLine
        , Enum.HaddockMultiLineCompact
        ]
      }

let ImportExportStyle =
      { name = "ImportExportStyle"
      , constructors =
        [ Enum.ImportExportLeading
        , Enum.ImportExportTrailing
        , Enum.ImportExportDiffFriendly
        ]
      }

let LetStyle =
      { name = "LetStyle"
      , constructors =
        [ Enum.LetAuto, Enum.LetInline, Enum.LetNewline, Enum.LetMixed ]
      }

let InStyle =
      { name = "InStyle"
      , constructors = [ Enum.InLeftAlign, Enum.InRightAlign ]
      }

let Unicode =
      { name = "Unicode"
      , constructors =
        [ Enum.UnicodeDetect, Enum.UnicodeAlways, Enum.UnicodeNever ]
      }

let EnumType = { name : Text, constructors : List Enum }

let ADT =
      { name : Text
      , constructors : List Text
      , parseJSON : Text
      , parsePrinterOptType : Text
      , cli : Text
      }

let FieldType = < Enum : EnumType | ADT : ADT >

let typeName =
      \(t : FieldType) ->
        merge { Enum = \(x : EnumType) -> x.name, ADT = \(x : ADT) -> x.name } t

let HaddockPrintStyleModule =
      { name = "HaddockPrintStyleModule"
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

let Option =
      { name : Text
      , fieldName : Text
      , description : Text
      , type : OptionType
      , default : Value
      , ormolu : Value
      , cli : Optional CLI
      }

let defaultCLI
    : Option -> CLITotal
    = \(option : Option) ->
        { help = option.description
        , default = showValuePretty option.default
        , placeholder = "OPTION"
        }

in  { showPlaceholder
    , showType
    , showEnum
    , showEnumPretty
    , showValue
    , showValuePretty
    , Option
    , Enum
    , ADT
    , EnumType
    , FieldType
    , typeName
    , options =
          [ { name = "indentation"
            , fieldName = "poIndentation"
            , description = "Number of spaces per indentation step"
            , type = OptionType.Natural
            , default = Value.Natural 4
            , ormolu = Value.Natural 2
            , cli = None CLI
            }
          , { name = "function-arrows"
            , fieldName = "poFunctionArrows"
            , description = "Styling of arrows in type signatures"
            , type = OptionType.Enum FunctionArrowsStyle
            , default = Value.Enum Enum.TrailingArrows
            , ormolu = Value.Enum Enum.TrailingArrows
            , cli = None CLI
            }
          , { name = "comma-style"
            , fieldName = "poCommaStyle"
            , description =
                "How to place commas in multi-line lists, records, etc."
            , type = OptionType.Enum CommaStyle
            , default = Value.Enum Enum.Leading
            , ormolu = Value.Enum Enum.Trailing
            , cli = None CLI
            }
          , { name = "import-export-style"
            , fieldName = "poImportExportStyle"
            , description = "Styling of import/export lists"
            , type = OptionType.Enum ImportExportStyle
            , default = Value.Enum Enum.ImportExportDiffFriendly
            , ormolu = Value.Enum Enum.ImportExportTrailing
            , cli = None CLI
            }
          , { name = "indent-wheres"
            , fieldName = "poIndentWheres"
            , description =
                "Whether to full-indent or half-indent 'where' bindings past the preceding body"
            , type = OptionType.Bool
            , default = Value.Bool False
            , ormolu = Value.Bool True
            , cli = None CLI
            }
          , { name = "record-brace-space"
            , fieldName = "poRecordBraceSpace"
            , description =
                "Whether to leave a space before an opening record brace"
            , type = OptionType.Bool
            , default = Value.Bool False
            , ormolu = Value.Bool True
            , cli = None CLI
            }
          , { name = "newlines-between-decls"
            , fieldName = "poNewlinesBetweenDecls"
            , description = "Number of spaces between top-level declarations"
            , type = OptionType.Natural
            , default = Value.Natural 1
            , ormolu = Value.Natural 1
            , cli = None CLI
            }
          , { name = "haddock-style"
            , fieldName = "poHaddockStyle"
            , description = "How to print Haddock comments"
            , type = OptionType.Enum HaddockPrintStyle
            , default = Value.Enum Enum.HaddockMultiLine
            , ormolu = Value.Enum Enum.HaddockSingleLine
            , cli = None CLI
            }
          , { name = "haddock-style-module"
            , fieldName = "poHaddockStyleModule"
            , description = "How to print module docstring"
            , type = OptionType.ADT HaddockPrintStyleModule
            , default = Value.Enum Enum.PrintStyleInherit
            , ormolu = Value.Enum Enum.PrintStyleInherit
            , cli = None CLI
            }
          , { name = "let-style"
            , fieldName = "poLetStyle"
            , description = "Styling of let blocks"
            , type = OptionType.Enum LetStyle
            , default = Value.Enum Enum.LetAuto
            , ormolu = Value.Enum Enum.LetInline
            , cli = None CLI
            }
          , { name = "in-style"
            , fieldName = "poInStyle"
            , description =
                "How to align the 'in' keyword with respect to the 'let' keyword"
            , type = OptionType.Enum InStyle
            , default = Value.Enum Enum.InRightAlign
            , ormolu = Value.Enum Enum.InRightAlign
            , cli = None CLI
            }
          , { name = "unicode"
            , fieldName = "poUnicode"
            , description = "Output Unicode syntax"
            , type = OptionType.Enum Unicode
            , default = Value.Enum Enum.UnicodeNever
            , ormolu = Value.Enum Enum.UnicodeNever
            , cli = None CLI
            }
          , { name = "respectful"
            , fieldName = "poRespectful"
            , description =
                "Give the programmer more choice on where to insert blank lines"
            , type = OptionType.Bool
            , default = Value.Bool True
            , ormolu = Value.Bool False
            , cli = None CLI
            }
          ]
        : List Option
    , fieldTypes =
          [ FieldType.Enum CommaStyle
          , FieldType.Enum FunctionArrowsStyle
          , FieldType.Enum HaddockPrintStyle
          , FieldType.ADT HaddockPrintStyleModule
          , FieldType.Enum ImportExportStyle
          , FieldType.Enum LetStyle
          , FieldType.Enum InStyle
          , FieldType.Enum Unicode
          ]
        : List FieldType
    }
