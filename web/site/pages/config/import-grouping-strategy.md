# `import-grouping-strategy`

$info$

The behavior depends on [`respectful`](/config/respectful):

- If set to `true`, the existing import groups are preserved, and each group are in turn regrouped with the requested strategy.
- If set to `false`, all existing import groups are merged and the result is regrouped with the requested strategy.

## Examples

```fourmolu-example-input
import Control.Monad (Monad (..))
import Data.Maybe (maybe)
import Data.Text (Text)
import qualified Data.Text
import SomeInternal.Module1.SubModuleA

import SomeInternal.Module1 (anotherDefinition, someDefinition)
import qualified SomeInternal.Module2 as Mod2
import qualified System.IO as SIO
import Text.Printf (printf)
```

```fourmolu-example-tab
none
{ "import-grouping-strategy": "none", "respectful": false }
```

```fourmolu-example-tab
by-qualified
{ "import-grouping-strategy": "by-qualified", "respectful": false }
```

```fourmolu-example-tab
by-scope
{ "import-grouping-strategy": "by-scope", "respectful": false }
```

```fourmolu-example-tab
by-scope-then-qualified
{ "import-grouping-strategy": "by-scope-then-qualified" , "respectful": false}
```

```fourmolu-example-tab
by-qualified-then-scope
{ "import-grouping-strategy": "by-qualified-then-scope" , "respectful": false}
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/import-grouping-strategy).

## See also

- [`respectful`](/config/respectful)
