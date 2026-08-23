module Main where

import Data.Either
import Data.Functor
import Foo
-- some comment
import Foo.Bar
import SomeInternal.Module2

import Data.Text (Text)
import qualified Data.Text

import Control.Monad (Monad (..))
import Data.Maybe (maybe)
import Text.Printf (printf)

import qualified SomeInternal.Module2 as Mod2

import SomeInternal.Module1 (anotherDefinition, someDefinition)
import qualified SomeModule
import qualified System.IO as SIO
