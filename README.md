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

Fourmolu is a formatter for Haskell source code. It is a fork of [Ormolu](https://github.com/mrkkrp/ormolu), with upstream improvements continually merged.

We share all bar one of Ormolu's goals:

* Use GHC's own parser to avoid the parsing problems caused by
  [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts).
* Make some whitespace programmable. The layout of the input influences the
  layout choices in the output, so the choice between single-line and
  multi-line layouts is made by the user rather than by an algorithm. This
  keeps the implementation simpler and leaves some control to the user while
  still guaranteeing that the formatted code is stylistically consistent.
* Produce minimal diffs.
* Choose a style compatible with modern dialects of Haskell. As new Haskell
  extensions enter broad use, we may adjust the style to accommodate them.
* Guarantee idempotence: formatting already formatted code doesn't change it.
* Stay well-tested and robust, so that the formatter can be used in large
  projects.
* ~~Implementing one “true” formatting style which admits no configuration.~~ We allow configuration of various parameters, via CLI options or config files. We encourage any contributions which add further flexibility.

## Configuration

See https://fourmolu.github.io/config/

## Installation

### (Recommended) Install with GHCup

```bash
ghcup config add-release-channel 3rdparty
ghcup install fourmolu latest
```

### Install with Cabal/Stack

```bash
cabal install fourmolu
stack install fourmolu
```

### Install with [dotslash](https://dotslash-cli.com/docs/)

Copy the configuration in the GitHub release notes.

## Building from source

```console
$ cabal build -fdev
$ stack build --flag fourmolu:dev
```

The `dev` flag may be omitted in your local workflow as you work, but CI may not pass if you only build without the `dev` flag.

## Usage

The following prints the formatted output to the standard output:

```console
$ fourmolu Module.hs
```

Add `-i` (or `--mode inplace`) to replace the contents of the input file with the formatted output:

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

To check whether files are already formatted (useful on CI):

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

### GitHub Actions

[`run-fourmolu`](https://github.com/haskell-actions/run-fourmolu) is the recommended way to ensure that a project stays formatted with Fourmolu.

### Language extensions, dependencies, and fixities

Fourmolu automatically locates the Cabal file that corresponds to a given
source file. Cabal files are used to extract both default extensions and
dependencies. Default extensions directly affect the behavior of the GHC
parser, while dependencies are used to determine the fixities of operators
that appear in the source code. Fixities can also be overridden via the `fixities` configuration option in `fourmolu.yaml`. When the input comes from stdin, you
can pass `--stdin-input-file` to tell Fourmolu which location to use as the
starting point when searching for `.cabal` files.

Here is an example of the `fixities` configuration:

```yaml
fixities:
  - infixr 9  .
  - infixr 5  ++
  - infixl 4  <$
  - infixl 1  >>, >>=
  - infixr 1  =<<
  - infixr 0  $, $!
  - infixl 4 <*>, <*, *>, <**>
  - infixr 3 >~<
  - infixr 3.3 |~|
  - infixr 3.7 <~>
```

It uses exactly the same syntax as ordinary Haskell fixity declarations,
which makes it easier for Haskellers to edit and maintain. Since Ormolu
0.7.8.0, fractional precedences are supported for more precise control over
the formatting of complex operator chains.

`fourmolu.yaml` can also contain instructions about
module re-exports that Fourmolu should be aware of. This can be useful because
Fourmolu cannot know about every possible module re-export in the ecosystem,
and only a few of them actually matter for fixity deduction. In 99% of cases
you won't have to do anything, especially since the most common re-exports
are already built into Fourmolu. (You are welcome to open PRs to make Fourmolu
aware of more re-exports by default.) However, when the fixity of an operator
is not inferred correctly, making Fourmolu aware of a re-export may help. Here
is an example:

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

These let you disable formatting selectively for the code between the two
markers, or for the entire file. To disable formatting for the whole file,
just put `{- FOURMOLU_DISABLE -}` at the very top. Note that the fragments
where Fourmolu is enabled must be parseable on their own. Because of this, the
magic comments cannot be placed arbitrarily; they must enclose independent
top-level definitions.

`{- ORMOLU_DISABLE -}` and `{- ORMOLU_ENABLE -}`, respectively, can be used to the same effect,
and the two styles of magic comments can be mixed.

### Regions

You can ask Fourmolu to format a region of the input and leave the rest
unformatted by passing the `--start-line` and `--end-line` command line
options. `--start-line` defaults to the beginning of the file, and
`--end-line` defaults to the end.

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
11        | Comments of original and formatted code differ
100       | In checking mode: unformatted files
101       | Inplace mode does not work with stdin
102       | Other issue (with multiple input files)
400       | Failed to load Fourmolu configuration file

### Using as a library

The `fourmolu` package can also be used as a dependency from other Haskell
programs. For this purpose, only the top-level `Ormolu` module should be
considered stable. It follows the [PVP](https://pvp.haskell.org/) starting
from version 0.10.2.0. Rely on other modules at your own risk.

## Troubleshooting

### Operators are being formatted weirdly!

This can happen when Ormolu doesn't know or can't determine the fixity of an
operator.

* If this is a custom operator, see the instructions in the [Language
  extensions, dependencies, and
  fixities](#language-extensions-dependencies-and-fixities) section to
  specify the correct fixities in a `fourmolu.yaml` file.

* If this is a third-party operator (e.g. from `base` or some other package
  on Hackage), Ormolu probably doesn't recognize that the operator is the
  same as the third-party one.

  Some possible reasons for this:

    * You have a custom Prelude that re-exports things from the standard
      Prelude.
    * You have `-XNoImplicitPrelude` turned on.

  If either of these applies, make sure to specify the re-exports correctly
  in a `fourmolu.yaml` file.

You can see how Ormolu decides the fixity of operators by using `--debug`.

## Limitations

* CPP support is experimental. CPP is virtually impossible to handle
  correctly, so Fourmolu treats CPP sections as unchangeable snippets. This
  works only in simple cases, where CPP conditionals surround top-level
  declarations. See the [CPP](https://github.com/mrkkrp/ormolu/blob/master/DESIGN.md#cpp) section of the design notes for a
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
