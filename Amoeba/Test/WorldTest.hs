{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Data.Monoid
import Control.Lens
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.Data
import Test.Utils.Arbitraries

import GameLogic.World.World
import GameLogic.World.Geometry
import GameLogic.World.Objects
import GameLogic.World.Properties

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

m = M.fromList [('a',1), ('b',2), ('c',3)]
k = S.fromList "bce"
r1 = m ^.. foldMap at k
r2 = m ^.. foldMap ix k

r3 = M.fromList [(1,"world")] ^.at 1
r4 = at 1 ?~ "hello" $ M.empty

prop_world1 p pl seed = game /= initialGame seed
    where
        game = world . worldMap . at p ?~ ps $ initialGame seed
        ps = plasma pl p

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."