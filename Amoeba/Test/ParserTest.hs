{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All

import GameLogic.Data.Facade
import GameLogic.Language.Parser

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

parseExampe dataFile p = do
    f <- readFile dataFile
    putStrLn $ "'" ++ f ++ "' -> "
    case p f of
        Left errMsg -> putStrLn errMsg
        x -> print x
    putStrLn ""

main :: IO ()
main = do
    --runTests
    
    parseExampe "./Raws/Items.its" parse
    parseExampe "./Raws/Item.its" parseItem
    
    putStrLn ""

