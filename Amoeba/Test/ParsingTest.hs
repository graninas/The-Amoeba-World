{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.All
import Control.Monad (liftM)

import GameLogic.Language.Parsing.ItemParser
import GameLogic.Language.Parsing.WorldParser
import GameLogic.Language.Parsing.RawParser
import GameLogic.Language.RawToken

import Test.Utils.WorldArfData

parseExample dataFile = liftM parseRawTokens (readFile dataFile)

testExample ex@(testName, dataFile, res) = do
    parsed <- parseExample dataFile
    return $ res == parsed

examineExample ex@(testName, dataFile, res) pred = do
    parsed <- parseExample dataFile
    return $ pred res parsed

prop_parseItems1 = monadicIO $ do
    res <- run $ testExample items1
    assert res
    
prop_parseItems2 = monadicIO $ do
    res <- run $ testExample items2
    assert res
    
prop_parseWorld1 = monadicIO $ do
    res <- run $ testExample world1
    assert res

prop_parseWorld2 = monadicIO $ do
    r <- run $ readFile "./Data/Raws/World2.adt"
    res <- run $ examineExample world2 (pred (read r))
    assert res
  where
    pred expected _ parsed = expected == parsed

prop_parseWorld3 = monadicIO $ do
    r <- run $ readFile "./Data/Raws/World3.adt"
    res <- run $ examineExample world3 (pred (read r))
    assert res
  where
    pred expected _ parsed = expected == parsed

writeAdt rawFile destFile = do
    p <- parseExample rawFile
    writeFile destFile $ show p

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    putStr ""
    --writeAdt "./Data/Raws/World3.arf" "./Data/Raws/World3.adt"
    