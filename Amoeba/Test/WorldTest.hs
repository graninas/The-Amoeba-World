{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Control.Lens
import Test.QuickCheck
import Test.QuickCheck.All

import World.World
import World.Geometry
import Test.Data

m = M.fromList [('a',1), ('b',2), ('c',3)]
k = S.fromList "bce"
r1 = m ^.. foldMap at k
r2 = m ^.. foldMap ix k

r3 = M.fromList [(1,"world")] ^.at 1
r4 = at 1 ?~ "hello" $ M.empty

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."