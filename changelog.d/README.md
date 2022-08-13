# Changelog

Every PR that makes a user-facing change (e.g. adding features, fixing bugs, etc.) should include a new `.md` file in this directory, containing:

* A quick summary of the change
* A link to the PR (the PR should link to any relevant issues)

## Release curation

Every release, all files in this directory (except for this file) should be curated into `CHANGELOG.md` and then deleted. The release notes for a new version `X.Y.Z` should be curated into a section in the following format:

```
## Fourmolu 1.2.3

* Fourmolu-specific change 1
* Fourmolu-specific change 2

### Upstream changes:

#### Ormolu 7.8.9.1

* Ormolu change 1

#### Ormolu 7.8.9.0

* Ormolu change 2
```
