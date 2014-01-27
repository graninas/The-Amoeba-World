{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All

import GameLogic.Data.Facade
import GameLogic.Language.Parser

parseExampe (testName, dataFile, res) p = do
    f <- readFile dataFile
    putStrLn $ "'" ++ f ++ "' -> "
    case p f of
        Left errMsg -> putStrLn errMsg
        x -> print (if x == res then "Test passed: " ++ testName
                    else "Test failed: " ++ testName)
    putStrLn ""

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

example1 = ("Example1", "./Raws/Items.its",
            Right [Comment " General items",EmptyToken,Item "Karyon" [IntResource "lifebound" (0,5000),IntResource "durability" (100,100),IntResource "energy" (300,2000)],EmptyToken,Comment " Conductor",Item "Conductor" [IntResource "lifebound" (0,1000),IntResource "durability" (100,100),IntResource "energy" (0,100)]]
            )


main :: IO ()
main = do
    runTests

    parseExampe example1 parse

    putStrLn ""

