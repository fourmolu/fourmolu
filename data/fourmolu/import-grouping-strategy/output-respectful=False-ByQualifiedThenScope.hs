module Main where

import Control.Monad (Monad (..))
import Data.Maybe (maybe)
import Data.Text (Text)
import Text.Printf (printf)

import SomeInternal.Module1 (anotherDefinition, someDefinition)

import qualified Data.Text
import qualified SomeModule
import qualified System.IO as SIO

import qualified SomeInternal.Module2 as Mod2
