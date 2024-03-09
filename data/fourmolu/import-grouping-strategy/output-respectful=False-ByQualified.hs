module Main where

import Control.Monad (Monad (..))
import Data.Maybe (maybe)
import Data.Text (Text)
import SomeInternal.Module1 (anotherDefinition, someDefinition)
import Text.Printf (printf)

import qualified Data.Text
import qualified SomeInternal.Module2 as Mod2
import qualified SomeModule
import qualified System.IO as SIO
