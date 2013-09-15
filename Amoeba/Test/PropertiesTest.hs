{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.All

import World.Player
import World.Properties
import World.Objects
import World.Geometry

import Test.Data
import Test.Arbitraries

p = Properties $ Map.fromList [(1, PDurability (0,0))]
p' = Properties $ Map.fromList [(1, PDurability (10,10))]


prop_durability (m, c) = ps /= emptyProperties
    where 
        st :: State Properties ()
        st = do v <- get
                durability .= (m, c)
        ps = execState st emptyProperties

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."