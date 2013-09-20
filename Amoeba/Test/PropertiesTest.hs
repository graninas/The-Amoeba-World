{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Default
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.All

import GameLogic.World.Player
import GameLogic.World.Properties
import GameLogic.World.Objects
import GameLogic.World.Geometry

import Test.Utils.Data
import Test.Utils.Arbitraries

-- Testing of Monoid laws for Properties

prop_monoidLaw1 ps = mappend mempty ps == ps
    where
        types = ps :: Properties

prop_monoidLaw2 ps = mappend ps mempty == ps
    where
        types = ps :: Properties

prop_monoidLaw3 ps1 ps2 ps3 = mappend ps1 (mappend ps2 ps3) == mappend (mappend ps1 ps2) ps3
    where
        types = [ps1, ps2, ps3] :: [Properties]

prop_monoidLaw4 pss = mconcat pss == foldr mappend mempty pss
    where
        types = pss :: [Properties]
        
tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests