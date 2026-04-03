module Main where

import Control.Monad (Monad (..))
import Data.Either
import Data.Functor
import Data.Maybe (maybe)
import Data.Text (Text)
import Text.Printf (printf)

import qualified Data.Text
import qualified SomeModule
import qualified System.IO as SIO

import SomeInternal.Module1 (anotherDefinition, someDefinition)
import SomeInternal.Module2

import qualified SomeInternal.Module2 as Mod2
