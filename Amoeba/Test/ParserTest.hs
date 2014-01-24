{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All

import GameLogic.Data.Facade
import GameLogic.Language.Parser

tokenEmptyComment = ";"
tokenCommentNoSpaces = ";This is comment and no spaces."
tokenComment = ";This is comment."

tokenItemNoResources = "Item \"ItemName\"\n"


tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    
    print "EmptyString:\n"
    print $ parseToken ""