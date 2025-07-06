module Main where

import Data.Maybe (maybe)
import Data.Text (Text)

import Control.Monad (Monad (..))
import qualified Data.Text

import SomeInternal.Module1 (anotherDefinition, someDefinition)
import qualified SomeInternal.Module2 as Mod2
import qualified SomeModule
import qualified System.IO as SIO
import Text.Printf (printf)
