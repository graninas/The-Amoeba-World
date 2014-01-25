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

itemItemNoResources = "Item \"ItemName\""
itemResource1 = unlines ["Item \"ItemName\"",
                         "    lifebound = (0, 5000)"]
itemResource3 = unlines ["Item \"ItemName\"",
                         "    lifebound = (0, 5000)",
                         "    durabilirty = (100, 100)",
                         "    energy = (300, 2000)"]

resource1 = "    lifebound = (10, 100)"
resource2 = "    durability = (100, 1000)"
resources2 = resource1 ++ "\n" ++ resource2
resources2_ = resource1 ++ "\n" ++ resource2 ++ "\n"



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
    
    printParsed parseResource resource1
    printParsed parseResource resource2
    printParsed parseResources resources2
    printParsed parseResources resources2_

    printParsed parseToken itemItemNoResources
    printParsed parseItem itemResource1
    printParsed parseItem itemResource3

    printParsed parseToken tokenEmptyComment
    printParsed parseToken tokenCommentNoSpaces
    printParsed parseToken tokenComment
