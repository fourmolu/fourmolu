let Prelude =
      https://prelude.dhall-lang.org/v17.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

-- let EnumConstructor = <
  -- split up in to separate actual types? how to sync up with `type` field?
let Enum = <
  | Leading
  | Trailing
  | TrailingArrows
  | LeadingArrows
  | LeadingArgsArrows
  | HaddockSingleLine
  | HaddockMultiLine
  | HaddockMultiLineCompact
  | PrintStyleInherit
  -- | PrintStyleOverride : HaddockPrintStyle
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

let Value = <
  | Bool : Bool
  | Natural : Natural
  | Text : Text
  | Enum : Enum
 >

let showEnum = \(x: Enum) -> merge
    --TODO use builtin

   {
        ,Leading = "Leading"
        ,Trailing = "Trailing"
        ,TrailingArrows = "TrailingArrows"
        ,LeadingArrows = "LeadingArrows"
        ,LeadingArgsArrows = "LeadingArgsArrows"
        ,HaddockSingleLine = "HaddockSingleLine"
        ,HaddockMultiLine = "HaddockMultiLine"
        ,HaddockMultiLineCompact = "HaddockMultiLineCompact"
        ,PrintStyleInherit = "PrintStyleInherit"
        ,ImportExportLeading = "ImportExportLeading"
        ,ImportExportTrailing = "ImportExportTrailing"
        ,ImportExportDiffFriendly = "ImportExportDiffFriendly"
        ,LetAuto = "LetAuto"
        ,LetInline = "LetInline"
        ,LetNewline = "LetNewline"
        ,LetMixed = "LetMixed"
        ,InLeftAlign = "InLeftAlign"
        ,InRightAlign = "InRightAlign"
        ,UnicodeDetect = "UnicodeDetect"
        ,UnicodeAlways = "UnicodeAlways"
        ,UnicodeNever = "UnicodeNever"
      }
      x

let showEnumPretty = \(x: Enum) -> merge
      {
        ,Leading = "leading"
        ,Trailing = "trailing"
        ,TrailingArrows = "trailing"
        ,LeadingArrows = "leading"
        ,LeadingArgsArrows = "leading-args"
        ,HaddockSingleLine = "single-line"
        ,HaddockMultiLine = "multi-line"
        ,HaddockMultiLineCompact = "multi-line-compact"
        ,PrintStyleInherit = "null" --TODO bit weird?
        ,ImportExportLeading = "leading"
        ,ImportExportTrailing = "trailing"
        ,ImportExportDiffFriendly = "diff-friendly"
        ,LetAuto = "auto"
        ,LetInline = "inline"
        ,LetNewline = "newline"
        ,LetMixed = "mixed"
        ,InLeftAlign = "left-align"
        ,InRightAlign = "right-align"
        ,UnicodeDetect = "detect"
        ,UnicodeAlways = "always"
        ,UnicodeNever = "never"
      }
      x

let showValue: Value -> Text = \(v: Value) -> merge
  {
    ,Bool = \(x: Bool) -> if x then "True" else "False"
    -- ,Bool = Prelude.Bool.show -- caps
    ,Natural = Natural/show
    ,Text = \(x: Text) -> x
    -- ,TextList = \(x: List Text) -> Prelude.Text.concatMapSep ", " Text (\(x: Text) -> x) x -- incomplete, could be simpler
    ,Enum = showEnum
  }
  v

--TODO DRY with above
let showValuePretty: Value -> Text = \(v: Value) -> merge
  {
    ,Bool = \(x: Bool) -> if x then "true" else "false"
    -- ,Bool = Prelude.Bool.show -- caps
    ,Natural = Natural/show
    ,Text = \(x: Text) -> x
    -- ,TextList = \(x: List Text) -> Prelude.Text.concatMapSep ", " Text (\(x: Text) -> x) x -- incomplete, could be simpler
    ,Enum = showEnumPretty
  }
  v

let CLI =
  { help : Optional Text,
    default_ : Optional Text,
    placeholder : Optional Text
  }
let CLITotal =
  { help : Text,
    default_ : Text,
    placeholder : Text
  }


--TODO name field matching variable name seems a little redundant
let CommaStyle = {
    ,name = "CommaStyle"
    ,constructors =
      [ ,Enum.Leading ,Enum.Trailing
        ]
  }
let FunctionArrowsStyle = {
    ,name = "FunctionArrowsStyle"
    ,constructors =
      [ ,Enum.TrailingArrows ,Enum.LeadingArrows ,Enum.LeadingArgsArrows
        ]
  }
let HaddockPrintStyle = {
    ,name = "HaddockPrintStyle"
    ,constructors =
      [ ,Enum.HaddockSingleLine ,Enum.HaddockMultiLine ,Enum.HaddockMultiLineCompact
        ]
  }
let ImportExportStyle = {
    ,name = "ImportExportStyle"
    ,constructors =
      [ ,Enum.ImportExportLeading ,Enum.ImportExportTrailing ,Enum.ImportExportDiffFriendly
        ]
  }
let LetStyle = {
    ,name = "LetStyle"
    ,constructors =
      [ ,Enum.LetAuto ,Enum.LetInline ,Enum.LetNewline ,Enum.LetMixed
        ]
  }
let InStyle = {
    ,name = "InStyle"
    ,constructors =
      [ ,Enum.InLeftAlign ,Enum.InRightAlign
        ]
  }
let Unicode = {
    ,name = "Unicode"
    ,constructors =
      [ ,Enum.UnicodeDetect ,Enum.UnicodeAlways ,Enum.UnicodeNever
        ]
  }


let EnumType = {
  ,name: Text
  ,constructors: List Enum
 }


let ADT = {
  ,name : Text
  ,constructors : List Text
  -- ,render : Text
  ,parseJSON : Text
  ,parsePrinterOptType : Text
  ,cli : Text
}

-- let FieldType = {
--   ,name: Text
--   ,def: Either
--  }
let FieldType = <
  | Enum: EnumType
  | ADT: ADT
>

let typeName = \(t : FieldType) -> merge
  {
    ,Enum = \(x: EnumType) -> x.name
    ,ADT = \(x: ADT) -> x.name
  }
  t

let HaddockPrintStyleModule = {
  ,name = "HaddockPrintStyleModule"
  ,constructors =
      [ "PrintStyleInherit"
      , "PrintStyleOverride HaddockPrintStyle"
      ]
  -- ,render =
  --       PrintStyleInherit: 'null'
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

let OptionType = <
  | Bool
  | Natural
  | Text
  | Enum : EnumType
  | ADT : ADT
 >

let showType = \(t: OptionType) -> merge
  {
  ,Bool = "Bool"
  ,Natural = "Int"
  ,Text = "Text"
  ,ADT = \(x: ADT) -> x.name
  ,Enum = \(x: EnumType) -> x.name
  }
  t

let showPlaceholder = \(t: OptionType) -> merge
  {
  ,Bool = "BOOL"
  ,Natural = "INT"
  ,Text = "TEXT"
  ,Enum = \(ft: EnumType) -> "OPTION"
  ,ADT = \(x: ADT) -> "OPTION"
  }
  t

let Option =
  { name : Text,
    fieldName : Text,
    -- fieldName : Optional Text,
    description : Text,
    type_ : OptionType,
    default_ : Value,
    ormolu : Value,
    cli : Optional  CLI --TODO never used
  }

-- #   * cli: (optional) Info for CLI
-- #       * help: (string, optional) Text to show in CLI help text; defaults to `description`
-- #       * default: (string, optional) Text to add to the CLI help text as the
-- #           default; defaults to rendered version of `default`
-- #       * placeholder: (string, optional) Placeholder to use in CLI help text;
-- #           by default, determined based on `type`

let defaultCLI: (Option -> CLITotal) = \(option: Option) ->
  {
    ,help =  option.description
    ,default_ = showValuePretty option.default_
    ,placeholder = "OPTION" --TODO
  }



-- data ADT = ADT
--   { constructors :: [Text],
--     render :: Text,
--     parseJSON :: Text,
--     parsePrinterOptType :: Text
--   }
--   deriving (Show, Generic, FromJSON)
-- -- type Enum_ = [(Text, Text)]
-- type Enum_ = Value

-- --   { enum
-- --   }
-- --   deriving (Show, Generic, FromJSON)

-- data FieldType = FieldType
--   { name :: Text,
--     enum :: Optional Enum_,
--     adt :: Optional ADT
--     --   , enum ::  Either Enum ADT
--   }
--   deriving (Show, Generic, FromJSON)

-- data Input = Input
--   { options :: [Option],
--     fieldTypes :: [FieldType]
--   }
--   deriving (Show, Generic, FromJSON)

-- # ----- Source of truth for configuration fields ----- #
-- #
-- # This file contains two sections: the first is the list of
-- # options available in Fourmolu configuration, and the second
-- # is the definition of new Haskell types.

-- # Each option contains the following fields:
-- #   * name: (string) Name of key in config file + flag in CLI
-- #   * fieldName: (string|null) Name of PrinterOpts field (or null if
-- #       config is not in PrinterOpts)
-- #   * description: (string) Description of option in Haddocks + CLI
-- #   * type_: (string) Haskell type of field
-- #   * default_: (string|int|bool|list) Fourmolu default, as Haskell value
-- #   * ormolu: (string|int|bool|list) Option that mimics Ormolu's formatting, as Haskell value
-- #   * cli: (optional) Info for CLI
-- #       * help: (string, optional) Text to show in CLI help text; defaults to `description`
-- #       * default_: (string, optional) Text to add to the CLI help text as the
-- #           default; defaults to rendered version of `default`
-- #       * placeholder: (string, optional) Placeholder to use in CLI help text;
-- #           by default, determined based on `type`


in {
  showPlaceholder,
  showType,
  showEnum,
  showEnumPretty,
  showValue,
  showValuePretty,
  Option,
  Enum,
  ADT,
  EnumType,
  FieldType,
  typeName,
  options = ([
 ,{
  , name = "indentation"
  , fieldName = "poIndentation"
  -- , fieldName = Some "poIndentation"
  , description = "Number of spaces per indentation step"
  , type_ = OptionType.Natural
  , default_ = Value.Natural 4
  , ormolu = Value.Natural 2
  , cli = None CLI
 },{
  , name = "function-arrows"
  , fieldName = "poFunctionArrows"
  -- , fieldName = Some "poFunctionArrows"
  , description = "Styling of arrows in type signatures"
  , type_ = OptionType.Enum FunctionArrowsStyle
  , default_ = Value.Enum Enum.TrailingArrows
  , ormolu = Value.Enum Enum.TrailingArrows
  , cli = None CLI
 },{
  , name = "comma-style"
  , fieldName = "poCommaStyle"
  -- , fieldName = Some "poCommaStyle"
  , description = "How to place commas in multi-line lists, records, etc."
  , type_ = OptionType.Enum CommaStyle
  , default_ = Value.Enum Enum.Leading
  , ormolu = Value.Enum Enum.Trailing
  , cli = None CLI
 },{
  , name = "import-export-style"
  , fieldName = "poImportExportStyle"
  -- , fieldName = Some "poImportExportStyle"
  , description = "Styling of import/export lists"
  , type_ = OptionType.Enum ImportExportStyle
  , default_ = Value.Enum Enum.ImportExportDiffFriendly
  , ormolu = Value.Enum Enum.ImportExportTrailing
  , cli = None CLI
 },{
  , name = "indent-wheres"
  , fieldName = "poIndentWheres"
  -- , fieldName = Some "poIndentWheres"
  , description = "Whether to full-indent or half-indent 'where' bindings past the preceding body"
  , type_ = OptionType.Bool
  , default_ = Value.Bool False
  , ormolu = Value.Bool True
  , cli = None CLI
 },{
  , name = "record-brace-space"
  , fieldName = "poRecordBraceSpace"
  -- , fieldName = Some "poRecordBraceSpace"
  , description = "Whether to leave a space before an opening record brace"
  , type_ = OptionType.Bool
  , default_ = Value.Bool False
  , ormolu = Value.Bool True
  , cli = None CLI
 },{
  , name = "newlines-between-decls"
  , fieldName = "poNewlinesBetweenDecls"
  -- , fieldName = Some "poNewlinesBetweenDecls"
  , description = "Number of spaces between top-level declarations"
  , type_ = OptionType.Natural
  , default_ = Value.Natural 1
  , ormolu = Value.Natural 1
  , cli = None CLI
 },{
  , name = "haddock-style"
  , fieldName = "poHaddockStyle"
  -- , fieldName = Some "poHaddockStyle"
  , description = "How to print Haddock comments"
  , type_ = OptionType.Enum HaddockPrintStyle
  , default_ = Value.Enum Enum.HaddockMultiLine
  , ormolu = Value.Enum Enum.HaddockSingleLine
  , cli = None CLI
 },{
  , name = "haddock-style-module"
  , fieldName = "poHaddockStyleModule"
  -- , fieldName = Some "poHaddockStyleModule"
  , description = "How to print module docstring"
  , type_ = OptionType.ADT HaddockPrintStyleModule
  , default_ = Value.Enum Enum.PrintStyleInherit
  , ormolu = Value.Enum Enum.PrintStyleInherit
  , cli = None CLI
 },{
  , name = "let-style"
  , fieldName = "poLetStyle"
  -- , fieldName = Some "poLetStyle"
  , description = "Styling of let blocks"
  , type_ = OptionType.Enum LetStyle
  , default_ = Value.Enum Enum.LetAuto
  , ormolu = Value.Enum Enum.LetInline
  , cli = None CLI
 },{
  , name = "in-style"
  , fieldName = "poInStyle"
  -- , fieldName = Some "poInStyle"
  , description = "How to align the 'in' keyword with respect to the 'let' keyword"
  , type_ = OptionType.Enum InStyle
  , default_ = Value.Enum Enum.InRightAlign
  , ormolu = Value.Enum Enum.InRightAlign
  , cli = None CLI
 },{
  , name = "unicode"
  , fieldName = "poUnicode"
  -- , fieldName = Some "poUnicode"
  , description = "Output Unicode syntax"
  , type_ = OptionType.Enum Unicode
  , default_ = Value.Enum Enum. UnicodeNever
  , ormolu = Value.Enum Enum. UnicodeNever
  , cli = None CLI
 },{
  , name = "respectful"
  , fieldName = "poRespectful"
  -- , fieldName = Some "poRespectful"
  , description = "Give the programmer more choice on where to insert blank lines"
  , type_ = OptionType.Bool
  , default_ = Value.Bool True
  , ormolu = Value.Bool False
  , cli = None CLI
--  },{
--   , name = "fixities"
--   , fieldName = None Text
--   , description = "Fixity information for operators"
--   , type_ = "[String]"
--   , default_ = Value.TextList ([] : List Text)
--   , ormolu = Value.TextList ([] : List Text)
--   , cli = None CLI

 }
]) : List Option
 , fieldTypes = [
    ,FieldType.Enum CommaStyle
    ,FieldType.Enum FunctionArrowsStyle
    ,FieldType.Enum HaddockPrintStyle
    ,FieldType.ADT HaddockPrintStyleModule
    ,FieldType.Enum ImportExportStyle
    ,FieldType.Enum LetStyle
    ,FieldType.Enum InStyle
    ,FieldType.Enum Unicode

 ] : List FieldType


}


-- : {Option : Type, options: List Option}

-- # Each fieldType contains the following fields:
-- #   * name: (string) Name of data type
-- #
-- # And exactly one of the following fields:
-- #   * enum: List of enum options, mapping constructor name to string representation
-- #   * adt: Information for a custom field type
-- #       * constructors: List of strings, one for each constructor
-- #       * render: Mapping from Haskell value to string representation
-- #       * parseJSON: Implementation of `Aeson.parseJSON`
-- #       * parsePrinterOptType: Implementation of `String -> Either String a`

--   -
--     name: HaddockPrintStyleModule
--     adt:
--       constructors:
--         - PrintStyleInherit
--         - PrintStyleOverride HaddockPrintStyle
--       render:
--         PrintStyleInherit: 'null'
--       parseJSON: |
--         \v -> case v of
--           Aeson.Null -> pure PrintStyleInherit
--           Aeson.String "" -> pure PrintStyleInherit
--           _ -> PrintStyleOverride <$> Aeson.parseJSON v
--       parsePrinterOptType: |
--         \s -> case s of
--           "" -> pure PrintStyleInherit
--           _ -> PrintStyleOverride <$> parsePrinterOptType s
