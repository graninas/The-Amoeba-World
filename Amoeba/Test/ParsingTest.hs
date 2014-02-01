{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.All

import GameLogic.Data.Facade
import GameLogic.Language.Parsers.ItemParser
import GameLogic.Language.Parsers.WorldParser

items1 = ("Items1", "./Data/Raws/Items.its",
         parseItemTokens,
         Right [Comment " General items",EmptyToken,Item "Karyon" [IntResource "lifebound" (0,5000),IntResource "durability" (100,100),IntResource "energy" (300,2000)],EmptyToken,Comment " Conductor",Item "Conductor" [IntResource "lifebound" (0,1000),IntResource "durability" (100,100),IntResource "energy" (0,100)]])

parseExample (testName, dataFile, parser, res) = do
    d <- readFile dataFile
    return $ parser d == res


prop_parseItems1 = monadicIO $ do
    res <- run $ parseExample items1
    assert res

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests


