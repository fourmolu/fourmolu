module Ormolu.CLI where

import Data.Aeson (camelTo2)
import Ormolu (ColorMode (..))
import Ormolu.Config (CommaStyle (..), HaddockPrintStyle (..))

----------------------------------------------------------------------------
-- Helpers

-- | Values that appear as arguments of CLI options and thus have
-- a corresponding textual representation.
class ToCLIArgument a where
  -- | Convert a value to its representation as a CLI option argument.
  toCLIArgument :: a -> String

  -- | Convert a value to its representation as a CLI option argument wrapped
  -- in apostrophes.
  toCLIArgument' :: a -> String
  toCLIArgument' x = "'" <> toCLIArgument x <> "'"

instance ToCLIArgument Bool where
  toCLIArgument True = "true"
  toCLIArgument False = "false"

instance ToCLIArgument CommaStyle where
  toCLIArgument Leading = "leading"
  toCLIArgument Trailing = "trailing"

instance ToCLIArgument Int where
  toCLIArgument = show

instance ToCLIArgument HaddockPrintStyle where
  toCLIArgument HaddockSingleLine = "single-line"
  toCLIArgument HaddockMultiLine = "multi-line"

instance ToCLIArgument ColorMode where
  toCLIArgument Never = "never"
  toCLIArgument Always = "always"
  toCLIArgument Auto = "auto"

toCLI :: String -> String
toCLI = camelTo2 '-' . drop 2
