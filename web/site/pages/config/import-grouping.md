# `import-grouping`

$info$

The behavior depends on [`respectful`](/config/respectful):

- If set to `true`, the existing import groups are preserved, and each group are in turn regrouped according to the import groups configuration.
- If set to `false`, all existing import groups are merged and the result is regrouped according to the import groups configuration.

Some basic presets are provided but you can configure your own rules via a dedicated YAML configuration. Here's a sample configuration:

```yaml
import-grouping:
  - name: "Text modules"
    rules:
      - glob: Data.Text
  - name: "The rest"
    preset: all
  - name: "My internals and monads unqualified"
    rules:
      - cabal: defined-modules
        qualified: no
      - glob: Control.Monad
        qualified: no
  - name: "My internals and monads qualified"
    rules:
      - cabal: defined-modules
        qualified: yes
      - glob: Control.Monad
        qualified: yes
  - name: "Specific monads"
    rules:
      - glob: Control.Monad.**
```

## Examples

```fourmolu-example-input
import Control.Monad (Monad (..))
import qualified Control.Monad.Error as Error
import Control.Monad.State.Lazy (MonadState (..))
import Data.Maybe (maybe)
import Data.Text (Text)
import Data.Text.IO (hGetLine)
import qualified Data.Text
import SomeInternal.Module1.SubModuleA

import SomeInternal.Module1 (anotherDefinition, someDefinition)
import qualified SomeInternal.Module2 as Mod2
import qualified System.IO as SIO
import Text.Printf (printf)
```

```fourmolu-example-tab
single
{ "import-grouping": "single", "respectful": false }
```

```fourmolu-example-tab
by-qualified
{ "import-grouping": "by-qualified", "respectful": false }
```

```fourmolu-example-tab
by-scope
{ "import-grouping": "by-scope", "respectful": false }
```

```fourmolu-example-tab
by-scope-then-qualified
{ "import-grouping": "by-scope-then-qualified", "respectful": false}
```

```fourmolu-example-tab
custom
{
  "import-grouping": [
    {
      "name": "Text modules",
      "rules": [{ "glob": "Data.Text" }]
    },
    {
      "name": "The rest",
      "preset": "all"
    },
    {
      "name": "My internals and monads unqualified",
      "rules": [
        {
          "cabal": "defined-modules",
          "qualified": false
        },
        {
          "glob": "Control.Monad",
          "qualified": false
        }
      ]
    },
    {
      "name": "My internals and monads qualified",
      "rules": [
        {
          "cabal": "defined-modules",
          "qualified": true
        },
        {
          "glob": "Control.Monad",
          "qualified": true
        }
      ]
    },
    {
      "name": "My internals and monads qualified",
      "rules": [
        {
          "glob": "Control.Monad.**"
        }
      ]
    }
  ],
  "respectful": false
}
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/import-grouping).

## See also

- [`respectful`](/config/respectful)