module Main where

import Data.Text (Text)
import Data.Maybe (maybe)

import qualified Data.Text
import Control.Monad (Monad (..))

import qualified System.IO as SIO
import SomeInternal.Module1 (anotherDefinition, someDefinition)
import qualified SomeInternal.Module2 as Mod2
import Text.Printf (printf)
import qualified SomeModule
