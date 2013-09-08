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
import qualified World.Items.Karyon as K

prop_emptyWorldMap iId seed p = let
    g = mkStdGen seed
    w = worldFromList [] iId g
    in null (itemsAt p w)

items1 :: Points -> [(Point, ActiveItem)]
items1 ps = let
    objects = concatMap (\(p, iId) -> S.stone iId P.stonePlayer p) (zip ps [1..])
    in packObjects objects

-- These tests are bad, but for education - OK.

prop_stepWithInactives seed ps = let
    g = mkStdGen seed
    w = worldFromList (items1 ps) (length ps) g
    (steppedW, anns) = stepWorld w
    its = worldItems w
    newIts = worldItems steppedW
    in L.sort newIts == L.sort its

setupKaryonGrowingTest seed p energy = let
    k = K.karyon 1 P.player1 energy p
    g = mkStdGen seed
    w = worldFromList (packObjects k) 1 g
    (steppedW, anns) = stepWorld w
    in (worldItems w, worldItems steppedW)

prop_karyonIsGrowing seed p = length newIts > length its
  where
    (its, newIts) = setupKaryonGrowingTest seed p 100
    
prop_karyonNotGrowing seed p = L.sort newIts == L.sort its
  where
    (its, newIts) = setupKaryonGrowingTest seed p 0  

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."