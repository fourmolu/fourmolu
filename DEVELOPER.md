# Fourmolu — Developer notes

## Contributing

Some things to keep in mind when making changes:

* Make the minimal amount of changes
    * Avoid refactoring where possible, don't reformat untouched code
    * Since we continuously merge in changes from Ormolu, reducing the number of potential conflicts goes a long way towards maintainability of this project.
    * This includes behavior changes that drastically change how `fourmolu` formats Fourmolu's source code itself
* Add a file to `changelog.d/` when applicable (see `changelog.d/README.md`)

### Running tests

The Fourmolu test suite contains the following types of tests:

1. Tests inherited from Ormolu
1. Unit tests specific for Fourmolu
1. Integration tests

All of these tests can be run with `cabal test` or `stack test`.

To regenerate the printer test outputs (`data/examples/**/*-four-out.hs`), set `ORMOLU_REGENERATE_EXAMPLES=1` in the environment before running `test`. Generally, this should not change any of the `*-out.hs` files, but it might change the `*-four-out.hs` files, if the setting in `defaultPrinterOpts` is different from the one in `fourmolu.yaml`

### Running `fourmolu`

After building from source (see `README.md`), you can run Fourmolu with

```bash
scripts/run-fourmolu.sh --mode=inplace ...
```

This script automatically detects whether you built `fourmolu` with Stack or Cabal. If the auto-detection isn't working out, you can override it by setting `export BUILD_TYPE={stack,cabal}` in your environment.

This is automatically run on Fourmolu's source code in the pre-commit hooks (see the "Pre-commit hooks" section) and is checked in CI. If you're not using the pre-commit hooks, use the above command to manually style the files you changed (see `.pre-commit-config.yaml` for the files to exclude).

### Pre-commit hooks

We highly recommend turning on pre-commit hooks to run checks every time you commit. To do so, install [`pre-commit`](https://pre-commit.com/) and run `pre-commit install` in this directory.

This is optional, but is run in CI regardless.

### Adding a new configuration option

Considering configurability is the raison d'être of Fourmolu, you're probably making a change that involves adding a new configuration option. Ideally, you've already opened an issue asking for thoughts on the new configuration. Assuming you've already done all that, here's a checklist to follow to ensure you've touched all the right places:

1. Add the configuration option to `PrinterOpts` in `Ormolu.Config.Types`
    * Follow all the compiler errors

1. Make the required changes to change styling based on the configuration option

1. Update the in-repo `fourmolu.yaml` with your configuration option set to most closely imitate Ormolu's default style

1. Add a test case to `Ormolu.Config.PrinterOptsSpec`
    * Add a corresponding `data/fourmolu/<label>/input.hs` file

1. Regenerate test outputs (see the "Running tests" section above)

1. Add your new option to the "Configuration" section in `README.md`
    * Both in the table and in the example `fourmolu.yaml` files

1. Add a file to `changelog.d/` (see `changelog.d/README.md`)

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

    1. Curate `CHANGELOG.md` (see `changelog.d/README.md`)

    1. Curate option order
        * Re-order the "Available options" table in the `README` with the options sorted by popularity/importance (using your best judgement, without too much churn every release)
        * Ensure the options are in the same order in the `fourmolu.yaml` file and `README` examples
        * Ensure the options are in the same order in `Ormolu.Config.Types`, `overFieldsM`, and `printerOptsMeta`
        * Ensure the `PrinterOptsSpec.hs` tests are also in the same order as the options

1. Create PR as usual and merge into `main`
    1. In the `check_sdist` CI job, check the output of the `stack sdist` step for any warnings.

1. Ensure your Hackage token is set in Settings > Secrets > Actions as `HACKAGE_TOKEN_<github_username>` (replace any non alphanumeric characters in username with `_`).
    * Generate a token from `https://hackage.haskell.org/user/<hackage_username>/manage`

1. Go to the GitHub Actions page, click on the "Release" workflow, and click "Run workflow" on the `main` branch

1. Publish the candidate: https://hackage.haskell.org/package/fourmolu/candidates

1. Publish the GitHub release: https://github.com/fourmolu/fourmolu/releases

1. If this is a new major version, update HLS to use it ([example](https://github.com/haskell/haskell-language-server/pull/2254)). It's rare that we'll be changing our API in a way that requires actual code changes.

## Merging upstream

Fourmolu aims to continue merging upstream changes in Ormolu. Whenever Ormolu makes a new release (ideally within a week), the following steps should be run to merge the changes into Fourmolu.

1. `cd` into your local copy of the Fourmolu repository
1. Add Ormolu as an upstream remote: `git remote add ormolu git@github.com:tweag/ormolu`
1. Check out a new branch: `git switch -c merge-ormolu`
1. Pull Ormolu's git history: `git fetch ormolu --no-tags`
1. Find the commit corresponding to the new Ormolu version and merge it: `git merge <commit> -m 'Merge ormolu-X.Y.Z'`
1. (Recommended) Switch to diff3 conflicts: `git checkout --conflict=diff3`. This provides more context that might be helpful for resolving conflicts. See [docs](https://git-scm.com/book/en/v2/Git-Tools-Advanced-Merging#_checking_out_conflicts).
1. Resolve conflicts + finish merge: `git merge --continue`
1. Run tests to ensure everything works well: `stack test`

### Resolving conflicts

* Conflicts at the following paths should be resolved by keeping the files DELETED (i.e. if there's a "deleted by us" conflict, use `git rm` to avoid adding the file to our repo):
    * `.github/workflows/binaries.yml`
    * `.buildkite/`
    * `CONTRIBUTING.md`
    * `DESIGN.md`
    * `format.sh`
    * `nix/`
    * `shell.nix`
    * `weeder.dhall`

* Conflicts at the following paths should be resolved by throwing out Ormolu's changes and keeping our changes (i.e. if there's a conflict, use `git checkout --ours`):
    * `stack.yaml`

* The state of the following paths should be the same as they are in Ormolu (i.e. if there's a conflict, use `git checkout --theirs`)
    * `expected-failures/`

* If `default.nix` is changed, manually verify that all end-to-end tests are accounted for. For example, `./region-tests/` is one directory of tests, which is captured in the `fourmolu:region-tests` test suite, where every test in `default.nix` has been ported into the Haskell test suite.

* Any Ormolu additions to `CHANGELOG.md` should NOT be kept, but instead be added to a new file in `changelog.d/` (e.g. named `ormolu-X.Y.Z`). See `changelog.d/README.md` for more details.

* Be careful when editing `fourmolu.cabal` to only change shared things (e.g. `tested-with`) and not Fourmolu things (e.g. `name` or `version`).

### Update tests

* Regenerate test files (see the "Running tests" section above)
* Remove any redundant Fourmolu output files
    ```bash
    ./scripts/clean_redundant_examples.py
    ```

## HLint

Ormolu isn't HLint-clean, so Fourmolu can't be fully.

If you're using HLS you may wish to disable HLint on this codebase entirely. In VSCode, for example, add `"haskell.plugin.hlint.diagnosticsOn": false` to `fourmolu/.vscode/settings.json`.
