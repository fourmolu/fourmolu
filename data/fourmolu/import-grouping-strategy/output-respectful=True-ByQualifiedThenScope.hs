module Main where

import Data.Maybe (maybe)
import Data.Text (Text)

import Control.Monad (Monad (..))

import qualified Data.Text

import Text.Printf (printf)

import SomeInternal.Module1 (anotherDefinition, someDefinition)

import qualified SomeModule
import qualified System.IO as SIO

import qualified SomeInternal.Module2 as Mod2
