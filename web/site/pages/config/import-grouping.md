# `import-grouping`

$info$

Some basic presets are provided but you can configure your own rules via a dedicated YAML configuration.

Predefined presets, usable directly as values for `import-grouping`:

- `legacy`: adopt the legacy behavior (before version 0.17), i.e., `single` when `respectful` is `false` and `preserve` when `respectful` is true. See [`respectful`](/config/respectful).
- `preserve`: preserve the existing import groups
- `single`: single group of imports
- `by-qualified`: one group for unqualified imports, then one group for qualified imports
- `by-scope`: one group for external imports, then one group for imports targeting modules from the current Cabal project
- `by-scope-then-qualified`: apply `by-scope` first, then `by-qualified`

When the value for `import-grouping` is a list, that list defines the order for groups of import declarations. Each group are defined by a name and a set of rules:

```yaml
name: "Text modules"
rules:
  - glob: Data.Text
```

Any import declaration matching at least one of those rules will belong to that group.

The following rule types are defined:

- `match: all`: matches all modules
- `match: local-modules`: matches modules defined in the current Cabal project. Those modules are automatically detected. Developers can add custom modules through the `--local-modules` CLI option. See [`local-modules`](/config/local-modules) for more information.
- `glob: <pattern>`: matches modules matching the provided `<pattern>`. `*` can be any character on the same module level. `**` can be any character and can span multiple module levels.

In addition to the type, certain attributes can be added to rules:

- `import-list: explicit | hiding | none`: when set, `explicit` only matches import declarations with an explicit import list, `hiding` only matches import declarations with the `hiding` clause, `none` only matches import declarations on a whole module (no explicit import list or `hiding` clause). When absent, import declarations will match regardless of import lists or `hiding` clauses.
- `qualified: <yes | no>`: when set, `yes` only matches import declarations that are qualified, unlike `no` which only matches import declarations that are not qualified.
- `priority: <int>`: in cases multiple rules from different groups match an import declaration, the value associated to `priority` is used as a tie-breaker: the matching rule with the lowest priority wins, and the import declaration will belong to the group with that rule.

Here's an example used in the `custom` configuration:

```yaml
import-grouping:
  - name: "Text modules"
    rules:
      - glob: Data.Text
  - name: "The rest"
    rules:
      - match: all
        priority: 100
  - name: "My internals and monads unqualified"
    rules:
      - match: local-modules
        qualified: no
      - glob: Control.Monad
        qualified: no
  - name: "My internals and monads qualified"
    rules:
      - match: local-modules
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
      "rules": [
        {
          "match": "all",
          "priority": 100
        }
      ]
    },
    {
      "name": "My internals and monads unqualified",
      "rules": [
        {
          "match": "local-modules",
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
          "match": "local-modules",
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

<!-- NOTE The configuration above is the JSON equivalent from the aforementioned YAML configuration. Please keep them in sync. -->

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/import-grouping).

## See also

- [`respectful`](/config/respectful)
