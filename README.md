# Fourmolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/fourmolu.svg?style=flat)](https://hackage.haskell.org/package/fourmolu)

* [Configuration](#configuration)
* [Installation](#installation)
* [Building from source](#building-from-source)
* [Usage](#usage)
    * [Editor integration](#editor-integration)
    * [Language extensions, dependencies, and fixities](#language-extensions-dependencies-and-fixities)
    * [Magic comments](#magic-comments)
    * [Regions](#regions)
    * [Exit codes](#exit-codes)
* [Limitations](#limitations)
* [Contributing](#contributing)
* [License](#license)

Fourmolu is a formatter for Haskell source code. It is a fork of [Ormolu](https://github.com/tweag/ormolu), with the intention to continue to merge upstream improvements.

We share all bar one of Ormolu's goals:

* Using GHC's own parser to avoid parsing problems caused by
  [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts).
* Let some whitespace be programmable. The layout of the input influences
  the layout choices in the output. This means that the choices between
  single-line/multi-line layouts in certain situations are made by the user,
  not by an algorithm. This makes the implementation simpler and leaves some
  control to the user while still guaranteeing that the formatted code is
  stylistically consistent.
* Writing code in such a way so it's easy to modify and maintain.
* That formatting style aims to result in minimal diffs.
* Choose a style compatible with modern dialects of Haskell. As new Haskell
  extensions enter broad use, we may change the style to accommodate them.
* Idempotence: formatting already formatted code doesn't change it.
* Be well-tested and robust so that the formatter can be used in large
  projects.
* ~~Implementing one “true” formatting style which admits no configuration.~~ We allow configuration of various parameters, via CLI options or config files. We encourage any contributions which add further flexibility.

## Configuration

### Available options

| Configuration option | Valid options | Description |
|----------------------|---------------|-------------|
| `indentation`        | any integer   | Number of spaces to use as indentation |
| `comma-style`        | `leading`, `trailing` | Where to put the comma in lists, tuples, etc. |
| `import-export-comma-style` | `leading`, `trailing` | Where to put the comma in import/export lists |
| `indent-wheres`      | `true`, `false` | `false` means save space by only half-indenting the `where` keyword |
| `record-brace-space` | `true`, `false` | `rec {x = 1}` vs `rec{x = 1}` |
| `diff-friendly-import-export` | `true`, `false` | Make multiline import/export lists as diff-friendly as possible (keeping the open-parentheses on the same line as the `import` line or not) |
| `respectful`         | `true`, `false` | Whether to respect user-specified newlines, e.g. in import groupings |
| `haddock-style`      | `multi-line`, `single-line` | Whether multiline haddocks should use `{-` or `--` |
| `newlines-between-decls` | any integer | number of newlines between top-level declarations |
| `fixities`           | A list of strings for defining fixities; see the "Language extensions, dependencies, and fixities" section below |

For examples of each of these options, see the [test files](https://github.com/fourmolu/fourmolu/tree/master/data/fourmolu/).

### Specifying configuration

Configuration options may be specified in either a `fourmolu.yaml` file or via command-line options. Fourmolu looks for a `fourmolu.yaml` file in all parents of the current directory, followed by [the XDG config directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:XdgConfig).

A complete configuration file, corresponding to Fourmolu's default options, looks like:

```yaml
indentation: 4
comma-style: leading
import-export-comma-style: leading
indent-wheres: false
record-brace-space: false
diff-friendly-import-export: true
respectful: true
haddock-style: multi-line
newlines-between-decls: 1
fixities: []
```

The configuration that most closely matches Ormolu's styling is:

```yaml
indentation: 2
comma-style: trailing
import-export-comma-style: trailing
indent-wheres: true
record-brace-space: true
diff-friendly-import-export: false
respectful: false
haddock-style: single-line
newlines-between-decls: 1
fixities: []
```

Command-line options override options in a configuration file. Run `fourmolu --help` to see all options.

## Installation

To install the latest release from Hackage, simply install with Cabal or Stack:

```console
$ cabal install fourmolu
$ stack install fourmolu
```

## Building from source

```console
$ cabal build -fdev
$ stack build --flag fourmolu:dev
```

The `dev` flag may be omitted in your local workflow as you work, but CI may not pass if you only build without the `dev` flag.

## Usage

The following will print the formatted output to the standard output.

```console
$ fourmolu Module.hs
```

Add `-i` (or `--mode inplace`) to replace the contents of the input file with the formatted output.

```console
$ fourmolu -i Module.hs
```

Use `find` to format a tree recursively:

```console
$ fourmolu -i $(find . -name '*.hs')
```

Or find all files in a project with `git ls-files`:

```console
$ fourmolu --mode inplace $(git ls-files '*.hs')
```

To check if files are are already formatted (useful on CI):

```console
$ fourmolu --mode check $(find . -name '*.hs')
```

#### :zap: Beware git's `core.autocrlf` on Windows :zap:
Fourmolu's output always uses LF line endings. In particular,
`fourmolu --mode check` will fail if its input is correctly formatted
*except* that it has CRLF line endings. This situation can happen on Windows
when checking out a git repository without having set [`core.autocrlf`](
https://www.git-scm.com/docs/git-config#Documentation/git-config.txt-coreautocrlf)
to `false`.

### Editor integration

Fourmolu can be integrated with your editor via the [Haskell Language Server](https://haskell-language-server.readthedocs.io/en/latest/index.html). Just set `haskell.formattingProvider` to `fourmolu` ([instructions](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#language-specific-server-options)).

### Language extensions, dependencies, and fixities

Fourmolu automatically locates the Cabal file that corresponds to a given
source code file. When input comes from stdin, one can pass
`--stdin-input-file` which will give Fourmolu the location of the Haskell
source file that should be used as the starting point for searching for a
suitable Cabal file. Cabal files are used to extract both default extensions
and dependencies. Default extensions directly affect behavior of the GHC
parser, while dependencies are used to figure out fixities of operators that
appear in the source code. Fixities can also be overridden with the `fixities` configuration option in `fourmolu.yaml`, e.g.

```yaml
fixities:
  - infixr 9  .
  - infixr 5  ++
  - infixl 4  <$
  - infixl 1  >>, >>=
  - infixr 1  =<<
  - infixr 0  $, $!
  - infixl 4 <*>, <*, *>, <**>
```

It uses exactly the same syntax as usual Haskell fixity declarations to make
it easier for Haskellers to edit and maintain.

Besides, all of the above-mentioned parameters can be controlled from the
command line:

* Language extensions can be specified with the `-o` or `--ghc-opt` flag.
* Dependencies can be specified with the `-p` or `--package` flag.
* Fixities can be specified with the `-f` or `--fixity` flag.

Searching for `.cabal` files can be disabled by passing
`--no-cabal`.

### Magic comments

Fourmolu understands two magic comments:

```haskell
{- FOURMOLU_DISABLE -}
```

and

```haskell
{- FOURMOLU_ENABLE -}
```

This allows us to disable formatting selectively for code between these
markers or disable it for the entire file. To achieve the latter, just put
`{- FOURMOLU_DISABLE -}` at the very top. Note that for Fourmolu to work the
fragments where Fourmolu is enabled must be parseable on their own. Because of
that the magic comments cannot be placed arbitrarily, but rather must
enclose independent top-level definitions.

`{- ORMOLU_DISABLE -}` and `{- ORMOLU_ENABLE -}`, respectively, can be used to the same effect,
and the two styles of magic comments can be mixed.

### Regions

One can ask Fourmolu to format a region of input and leave the rest
unformatted. This is accomplished by passing the `--start-line` and
`--end-line` command line options. `--start-line` defaults to the beginning
of the file, while `--end-line` defaults to the end.

### Exit codes

Exit code | Meaning
----------|-----------------------------------------------
0         | Success
1         | General problem
2         | CPP used (deprecated)
3         | Parsing of original input failed
4         | Parsing of formatted code failed
5         | AST of original and formatted code differs
6         | Formatting is not idempotent
7         | Unrecognized GHC options
8         | Cabal file parsing failed
9         | Missing input file path when using stdin input and accounting for .cabal files
10        | Parse error while parsing fixity overrides
100       | In checking mode: unformatted files
101       | Inplace mode does not work with stdin
102       | Other issue (with multiple input files)
400       | Failed to load Fourmolu configuration file

## Limitations

* CPP support is experimental. CPP is virtually impossible to handle
  correctly, so we process them as a sort of unchangeable snippets. This
  works only in simple cases when CPP conditionals surround top-level
  declarations. See the [CPP](https://github.com/tweag/ormolu/blob/master/DESIGN.md#cpp) section in the design notes for a
  discussion of the dangers.
* Input modules should be parsable by Haddock, which is a bit stricter
  criterion than just being valid Haskell modules.
* Various minor idempotence issues, most of them are related to comments.
* Fourmolu is in a fairly early stage of development. The implementation should be as stable as Ormolu, as it only makes minimal changes, and is extensively tested. But the default configuration style may change in some minor ways in the near future, as we make more options available. It will always be possible to replicate the old default behaviour with a suitable `fourmolu.yaml`.

## Contributing

If there are any options you'd like to see, let us know. If it's not too complicated to implement (and especially if you implement it yourself!) then we'll probably add it.

See `DEVELOPER.md` for documentation.

## License

See [LICENSE.md](LICENSE.md).

Copyright © 2018–2020 Tweag I/O, 2020-present Matt Parsons

## Acknowledgements

The vast majority of work here has been done by the Ormolu developers, and thus they deserve almost all of the credit. This project is simply intended as a haven for those of us who admire their work, but can't quite get on board with some of their decisions when it comes down to the details.
