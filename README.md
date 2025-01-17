# Fourmolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/fourmolu.svg?style=flat)](https://hackage.haskell.org/package/fourmolu)
[![CI](https://github.com/fourmolu/fourmolu/actions/workflows/ci.yml/badge.svg)](https://github.com/fourmolu/fourmolu/actions/workflows/ci.yml)

* [Configuration](#configuration)
* [Installation](#installation)
* [Building from source](#building-from-source)
* [Usage](#usage)
    * [Web app](#web-app)
    * [Editor integration](#editor-integration)
    * [Language extensions, dependencies, and fixities](#language-extensions-dependencies-and-fixities)
    * [Magic comments](#magic-comments)
    * [Regions](#regions)
    * [Exit codes](#exit-codes)
    * [Using as a library](#using-as-a-library)
* [Troubleshooting](#troubleshooting)
    * [Operators are being formatted weirdly!](#operators-are-being-formatted-weirdly)
* [Limitations](#limitations)
* [Breaking changes policy](#breaking-changes-policy)
* [Contributing](#contributing)
* [License](#license)

Fourmolu is a formatter for Haskell source code. It is a fork of [Ormolu](https://github.com/tweag/ormolu), with upstream improvements continually merged.

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

See https://fourmolu.github.io/config/

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

Specify a directory to recursively process all of its `.hs` files:

```console
$ fourmolu -i src
```

Or find all files in a project with `git ls-files`:

```bash
$ fourmolu --mode inplace $(git ls-files '*.hs')
# Or to avoid hitting command line length limits and enable parallelism (12-way here):
$ git ls-files -z '*.hs' | xargs -P 12 -0 fourmolu --mode inplace
```

To check if files are already formatted (useful on CI):

```console
$ fourmolu --mode check src
```

#### :zap: Beware git's `core.autocrlf` on Windows :zap:
Fourmolu's output always uses LF line endings. In particular,
`fourmolu --mode check` will fail if its input is correctly formatted
*except* that it has CRLF line endings. This situation can happen on Windows
when checking out a git repository without having set [`core.autocrlf`](
https://www.git-scm.com/docs/git-config#Documentation/git-config.txt-coreautocrlf)
to `false`.

### Web app

See https://fourmolu.github.io/ to try Fourmolu in your browser. This is re-deployed on every new commit to `main`, so will use the latest version of Fourmolu, potentially including unreleased changes.

### Editor integration

Fourmolu can be integrated with your editor via the [Haskell Language Server](https://haskell-language-server.readthedocs.io/en/latest/index.html). Just set `haskell.formattingProvider` to `fourmolu` ([instructions](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#language-specific-server-options)).

### GitHub actions

[`run-fourmolu`][https://github.com/haskell-actions/run-fourmolu] is the recommended way to ensure that a project is formatted with Fourmolu.

### Language extensions, dependencies, and fixities

Fourmolu automatically locates the Cabal file that corresponds to a given
source code file. Cabal files are used to extract both default extensions
and dependencies. Default extensions directly affect behavior of the GHC
parser, while dependencies are used to figure out fixities of operators that
appear in the source code. Fixities can also be overridden via the `fixities` configuration option in `fourmolu.yaml`. When the input comes from
stdin, one can pass `--stdin-input-file` which will give Fourmolu the location
that should be used as the starting point for searching for `.cabal` files.

Here is an example of the `fixities` configuration:

```haskell
infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!
infixl 4 <*>, <*, *>, <**>

infixr 3 >~<
infixr 3.3 |~|
infixr 3.7 <~>
```

It uses exactly the same syntax as usual Haskell fixity declarations to make
it easier for Haskellers to edit and maintain.

`fourmolu.yaml` can also contain instructions about
module re-exports that Fourmolu should be aware of. This might be desirable
because at the moment Fourmolu cannot know about all possible module
re-exports in the ecosystem and only few of them are actually important when
it comes to fixity deduction. In 99% of cases the user won't have to do
anything, especially since most common re-exports are already programmed
into Fourmolu. (You are welcome to open PRs to make Fourmolu aware of more
re-exports by default.) However, when the fixity of an operator is not
inferred correctly, making Fourmolu aware of a re-export may come in handy.
Here is an example:

```yaml
reexports:
  - module Control.Lens exports Control.Lens.At
  - module Control.Lens exports "lens" Control.Lens.Lens
```

Explicit package names are allowed in re-export declarations (see the example above).

Finally, all of the above-mentioned parameters can be controlled from the
command line:

* Language extensions can be specified with the `-o` or `--ghc-opt` flag.
* Dependencies can be specified with the `-p` or `--package` flag.
* Fixities can be specified with the `-f` or `--fixity` flag.
* Re-exports can be specified with the `-r` or `--reexport` flag.

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

Note that the selected region needs to be parseable Haskell code on its own.

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

### Using as a library

The `fourmolu` package can also be depended upon from other Haskell programs.
For these purposes only the top `Ormolu` module should be considered stable.
It follows [PVP](https://pvp.haskell.org/) starting from the version
0.10.2.0. Rely on other modules at your own risk.

## Troubleshooting

### Operators are being formatted weirdly!

This can happen when Ormolu doesn't know or can't determine the fixity of an
operator.

* If this is a custom operator, see the instructions in the [Language
  extensions, dependencies, and
  fixities](#language-extensions-dependencies-and-fixities) section to
  specify the correct fixities in a `fourmolu.yaml` file.

* If this is a third-party operator (e.g. from `base` or some other package
  from Hackage), Ormolu probably doesn't recognize that the operator is the
  same as the third-party one.

  Some reasons this might be the case:

    * You might have a custom Prelude that re-exports things from Prelude
    * You might have `-XNoImplicitPrelude` turned on

  If any of these are true, make sure to specify the reexports correctly in
  a `fourmolu.yaml` file.

You can see how Ormolu decides the fixity of operators if you use `--debug`.

## Limitations

* CPP support is experimental. CPP is virtually impossible to handle
  correctly, so we process them as a sort of unchangeable snippets. This
  works only in simple cases when CPP conditionals surround top-level
  declarations. See the [CPP](https://github.com/tweag/ormolu/blob/master/DESIGN.md#cpp) section in the design notes for a
  discussion of the dangers.
* Various minor idempotence issues, most of them are related to comments or column limits.

## Breaking changes policy

Fourmolu is still in a relatively early stage of development, but it is in wide enough use that stability is a desirable property. Fourmolu aims to uphold the following principles:

1. It will always be possible to replicate Ormolu's formatting with a suitable `fourmolu.yaml`

1. Breaking changes will be avoided where possible, but may still occur in the following circumstances:

  * Fourmolu inherits a breaking change from Ormolu
  * The change reverts a prior breaking change, which caused a regression
  * Other exceptional situations, on a case-by-case basis

## Contributing

If there are any options you'd like to see, let us know. If it's not too complicated to implement (and especially if you implement it yourself!) then we'll probably add it.

See `DEVELOPER.md` for documentation.

## License

See [LICENSE.md](LICENSE.md).

Copyright © 2018–2020 Tweag I/O, 2020-present Matt Parsons

## Acknowledgements

The vast majority of work here has been done by the Ormolu developers, and thus they deserve almost all of the credit. This project is simply intended as a haven for those of us who admire their work, but can't quite get on board with some of their decisions when it comes down to the details.
