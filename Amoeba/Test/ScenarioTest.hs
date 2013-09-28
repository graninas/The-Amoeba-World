{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Default
import Data.Maybe
import Data.Char
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State

import Test.QuickCheck
import Test.QuickCheck.All

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests