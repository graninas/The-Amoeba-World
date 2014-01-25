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

itemResource1 = unlines ["Item \"ItemName1\"",
                         "    lifebound = (0, 5000)"]
itemResource3 = unlines ["Item \"ItemName2\"",
                         "    lifebound = (0, 1000)",
                         "    durabilirty = (10, 50)",
                         "    energy = (300, 2000)"]

items1 = itemResource1 ++ "\n" ++ itemResource3
items2 = itemResource1 ++ "\n" ++ tokenComment ++ "\n" ++ itemResource3

example = unlines [ "; General items"
                  , ""
                  , "Item \"Karyon\""
                  , "    lifebound = (0, 5000)"
                  , "    durabilirty = (100, 100)"
                  , "    energy = (300, 2000) "
                  , ""
                  , "; Conductor"
                  , "Item \"Conductor\""
                  , "    lifebound = (0, 1000)"
                  , "    durability = (100, 100)"
                  , "    energy = (0, 100)"
                  ]

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

printParsed p str = do
    putStrLn $ "'" ++ str ++ "' -> "
    case p str of
        Left errMsg -> putStrLn errMsg
        x -> print x
    putStrLn ""

main :: IO ()
main = do
    runTests
    
    printParsed parse example
    {-
    printParsed parseTokens items1
    printParsed parseTokens items2

    printParsed parseItem itemResource1
    printParsed parseItem itemResource3
    printParsed parseToken tokenEmptyComment
    printParsed parseToken tokenCommentNoSpaces
    printParsed parseToken tokenComment
    -}