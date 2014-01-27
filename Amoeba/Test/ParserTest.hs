{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All

import GameLogic.Data.Facade
import GameLogic.Language.Parser

prop_parseTest = case parse example of
    Left _ -> False
    Right ts -> length ts == 4


tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

printParsed dataFile p = do
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
    
    putStrLn ""

