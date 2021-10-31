# Fourmolu — Developer notes

## Contributing

Some things to keep in mind when making changes:

* Make the minimal amount of changes
    * Avoid refactoring where possible, don't reformat untouched code
    * Since we continuously merge in changes from Ormolu, reducing the number of potential conflicts goes a long way towards maintainability of this project.

## Instant feedback with GHCID

We often want to immediately see how changes to Fourmolu's source code affect outputs. Try adding something like this to `Ormolu.hs`:

```hs
import qualified Data.Text.IO as T
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  dir <- (</> "Desktop") <$> getHomeDirectory
  ormoluFile conf (dir </> "In.hs") >>= T.writeFile (dir </> "Out.hs")
  where
    conf =
      defaultConfig
        { cfgUnsafe = True,
          cfgPrinterOpts =
            defaultPrinterOpts
              { poCommaStyle = pure Trailing
              }
        }
```

Put some interesting code in `In.hs`. The contents of `Out.hs` can be kept up to date to reflect the result of running Fourmolu on it, by running:

```
ghcid -c 'cabal repl' -W -r --reload=$HOME/Desktop/In.hs
```

## Release a new version

To release a new version, do the following workflow:

1. Create a new branch

    1. Bump version in `fourmolu.cabal`
        * All version bumps should follow [PvP](https://pvp.haskell.org/)

    1. Curate `CHANGELOG.md`, creating a new section for this version and moving everything previously in `Unreleased` into the new section (keeping `Unreleased` as a section)

       It should roughly follow this format:

       ```md
       ## Fourmolu 1.2.3.0

       * Fourmolu-specific change 1
       * Fourmolu-specific change 2

       ### Upstream changes:

       #### Ormolu 2.3.4.0

       * Ormolu change 1

       #### Ormolu 2.3.3.0

       * Other change
       ```

    1. Add comments to new features indicating when it was added (e.g. `@since v2.0.0`)

    1. Run `stack haddock` or `cabal haddock` and skim through documentation

1. Create PR as usual and merge into `master`
    1. In the `test_latest` CI job, check the output of the `stack sdist` step for any warnings.

1. Upload the package to Hackage
    1. Download the `fourmolu-*.tar.gz` file from CI artifacts
    1. Upload tarball to Hackage

1. If this is a new major version, update HLS to use it ([example](https://github.com/haskell/haskell-language-server/pull/2254)). It's rare that we'll be changing our API in a way that requires actual code changes.

## Merging upstream

Fourmolu aims to continue merging upstream changes in Ormolu. Whenever Ormolu makes a new release (ideally within a week), the following steps should be run to merge the changes into Fourmolu.

1. `cd` into your local copy of the Fourmolu repository
1. Add Ormolu as an upstream remote: `git remote add ormolu git@github.com:tweag/ormolu`
1. Check out a new branch: `git switch -c merge-ormolu`
1. Pull Ormolu's git history: `git fetch ormolu --no-tags`
1. Find the commit corresponding to the new Ormolu version and merge it: `git merge <commit>`
1. (Recommended) Switch to diff3 conflicts: `git checkout --conflict=diff3`. This provides more context that might be helpful for resolving conflicts. See [docs](https://git-scm.com/book/en/v2/Git-Tools-Advanced-Merging#_checking_out_conflicts).
1. Resolve conflicts + finish merge: `git merge --continue`
    * Rewrite the default commit message to "Merge ormolu-X.Y.Z"
1. Run tests to ensure everything works well: `stack test`

### Resolving conflicts

* Conflicts at the following paths should be resolved by keeping the files DELETED (i.e. if there's a "deleted by us" conflict, use `git rm` to avoid adding the file to our repo):
    * `.github/`
    * `.buildkite/`
    * `DESIGN.md`
    * `default.nix`
    * `nix/`
    * `shell.nix`

* Conflicts at the following paths should be resolved by throwing out Ormolu's changes and keeping our changes (i.e. if there's a conflict, use `git checkout --ours`):
    * `stack.yaml`

* The state of the following paths should be the same as they are in Ormolu (i.e. if there's a conflict, use `git checkout --theirs`)
    * `expected-failures/`

* Any Ormolu additions to `CHANGELOG.md` should be added under a `### Upstream changes:` header in the `Unreleased` section of `CHANGELOG.md`, with the Ormolu headers bumped to `####`. See the CHANGELOG format in the "Release a new version" section.

* Be careful when editing `fourmolu.cabal` to only change shared things (e.g. `tested-with`) and not Fourmolu things (e.g. `name` or `version`).

### Update tests

* Regenerate test files

    1. Comment out the line in `PrinterSpec.hs` after the "UNCOMMENT NEXT LINE TO REGENERATE OUTPUT FILES" comment
    1. Run tests and commit any new `*-four-out.hs` files

* Remove any redundant Fourmolu output files

    ```bash
    find data/examples -name '*-four-out.hs' -print0 | while IFS= read -r -d '' f; do
        dir=$(dirname "$f")
        name=$(basename "$f" '-four-out.hs')
        src="${dir}/${name}.hs"
        if [[ ! -f "$src" ]]; then
            rm -v "$f"
        fi
    done
    ```

## HLint

Ormolu isn't HLint-clean, so Fourmolu can't be fully.

If you're using HLS you may wish to disable HLint on this codebase entirely. In VSCode, for example, add `"haskell.plugin.hlint.diagnosticsOn": false` to `fourmolu/.vscode/settings.json`.
