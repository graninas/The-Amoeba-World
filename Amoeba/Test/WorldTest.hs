{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import System.Random
import qualified Data.List as L

import Test.Dummy
import Test.Arbitraries
import Test.Data

import World.World
import World.Geometry
import qualified World.Player as P
import qualified World.Items.Stone as S
import qualified World.Items.Border as B
import qualified World.Items.Canyon as C 

prop_emptyWorldMap iId seed p = let
    g = mkStdGen seed
    w = worldFromList [] iId g
    in null (itemsAt p w)

items1 :: Points -> [(Point, ActiveItem)]
items1 ps = let
    objects = concatMap (\(p, iId) -> S.stone iId P.stonePlayer p) (zip ps [1..])
    in packObjects objects

prop_stepWithInactives seed ps = let
    g = mkStdGen seed
    its = items1 ps
    w = worldFromList its (length ps) g
    (steppedW, anns) = stepWorld w
    newIts = worldItems steppedW
    in L.sort newIts == L.sort (map snd its)

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."