{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.Data
import Test.Utils.Arbitraries

{-
world1:
karyon point1 player1 0

step world1 -> world2:
karyon point1 player1
plasma (around point1) player1

step world2 -> world3:
karyon point1 player1
plasma (around point1) player1
plasma (around (around point1)) player1
-}






tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    
    print $ soundWaveFabric player1 left point1