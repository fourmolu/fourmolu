# Fourmolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/fourmolu.svg?style=flat)](https://hackage.haskell.org/package/fourmolu)

Fourmolu is a formatter for Haskell source code. It is a fork of [Ormolu](https://github.com/tweag/ormolu), with the intention to continue to merge upstream improvements.

We share all bar one of Ormolu's goals:

* Using GHC's own parser to avoid parsing problems caused by
  [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts).
* Let some whitespace be programmable. The layout of the input influences
  the layout choices in the output. This means that the choices between
  single-line/multi-line layouts in each particular situation are made by
  the user, not by an algorithm. This makes the implementation simpler and
  leaves some control to the user while still guaranteeing that the
  formatted code is stylistically consistent.
* Writing code in such a way so it's easy to modify and maintain.
* That formatting style aims to result in minimal diffs while still
  remaining very close to “conventional” Haskell formatting people use.
* Choose a style compatible with modern dialects of Haskell. As new Haskell
  extensions enter broad use, we may change the style to accomodate them.
* Idempotence: formatting already formatted code doesn't change it.
* Be well-tested and robust to the point that it can be used in large
  projects without exposing unfortunate, disappointing bugs here and there.
* ~~Implementing one “true” formatting style which admits no configuration.~~ We allow configuration of various parameters, via CLI options or config files. We encourage contributions which add further flexibility, so long as they don't unduly complicate the implementation.

## Configuration

The current options affecting formatting are:
* `indentation`, taking any integer value

Fourmolu looks for a `fourmolu.yaml` file in all parents of the current directory, followed by [the XDG config directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:XdgConfig). A complete configuration file, corresponding to Fourmolu's default options, looks like:

```yaml
indentation: 4
```

See [here](fourmolu.yaml) for a config to simulate the behaviour of Ormolu.

These options can also be set on the command line (which takes precedence over config files). Run `fourmolu -h` to see all options.

## Building

Simply run `cabal v2-install fourmolu`, to install the latest release from Hackage.

You can also clone this repository, then build with Cabal or Stack.

## Usage

The following will print the formatted output to the standard output.

```console
$ fourmolu Module.hs
```

Add `-i` (or `--mode inplace`) to replace the contents of the input file with the formatted output.

```console
$ ormolu -i Module.hs
```

Use `find` to format a tree recursively:

```console
$ ormolu -i $(find . -name '*.hs')
```

Fourmolu can be integrated with your editor via the [Haskell Language Server](https://github.com/haskell/haskell-language-server).

## Magic comments

Fourmolu understands two magic comments:

```haskell
{- ORMOLU_DISABLE -}
```

and

```haskell
{- ORMOLU_ENABLE -}
```

This allows us to disable formatting selectively for code between these
markers or disable it for the entire file. To achieve the latter, just put
`{- ORMOLU_DISABLE -}` at the very top. Note that the source code should
still be parseable even without the “excluded” part. Because of that the
magic comments cannot be placed arbitrary, but should rather enclose
independent top-level definitions.

## Current limitations

* CPP support is experimental. CPP is virtually impossible to handle
  correctly, so we process them as a sort of unchangeable snippets. This
  works only in simple cases when CPP conditionals surround top-level
  declarations. See the [CPP][design-cpp] section in the design notes for
  a discussion of the dangers.
* Input modules should be parsable by Haddock, which is a bit stricter
  criterion than just being valid Haskell modules.
* Various minor idempotence issues, most of them are related to comments.
* Fourmolu is in a fairly early stage of development. The implementation should be as stable as Ormolu, as it only makes minimal changes, and is extensively tested. But the default configuration style may change in some minor ways in the near future, as we make more options available. It will always be possible to replicate the old default behaviour with a suitable `fourmolu.yaml`.

## Contributing

See [CONTRIBUTING.md][contributing].

## License

See [LICENSE.md][license].

Copyright © 2018–2020 Tweag I/O, 2020-present Matt Parsons

## Acknowledgements

The vast majority of work here has been done by the Ormolu developers, and thus they deserve almost all of the credit. This project is simply intended as a haven for those of us who admire their work, but can't quite get on board with some of their decisions when it comes down to the details.
