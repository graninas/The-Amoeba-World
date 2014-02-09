{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.All
import Control.Monad (liftM)

import GameLogic.Language.RawToken
import GameLogic.Language.Parsers.ItemParser
import GameLogic.Language.Parsers.WorldParser
import GameLogic.Language.Parsers.RawParser

-- 'ARF' stands for 'Amoeba Raw File' or 'Amoeba Raw Format' if you wish.

items1 = ("Items1", "./Data/Raws/Items.arf",
         parseRawTokens,
         Right [Comment " General items",EmptyToken,Item "Karyon" [IntResource "lifebound" (0,5000),IntResource "durability" (100,100),IntResource "energy" (300,2000)],EmptyToken,Comment " Conductor",Item "Conductor" [IntResource "lifebound" (0,1000),IntResource "durability" (100,100),IntResource "energy" (0,100)]])
items2 = ("Items2", "./Data/Raws/Item.arf",
         parseRawTokens,
         Right [Item "Karyon" [IntResource "lifebound" (0,5000), IntResource "durability" (100,100), IntResource "energy" (300,2000)]])
world1 = ("World1", "./Data/Raws/World1.arf",
         parseRawTokens,
         Right [ Comment " World definition file"
               , EmptyToken
               , World "Pandora" [ IntProperty "width" 20, IntProperty "height" 20, ObjectProperty "defaultCell" (Object "Empty" "Player0")
                                 , CellsProperty "cells" [ CellProperty (10, 10) (Object "Karyon" "Player1")
                                                         , CellProperty (9, 9) (Object "Plasma" "Player1")]]
               ])

parseExample parser dataFile = liftM parser (readFile dataFile) 

testExample ex@(testName, dataFile, parser, res) = do
    parsed <- parseExample parser dataFile
    return $ res == parsed

prop_parseItems1 = monadicIO $ do
    res <- run $ testExample items1
    assert res
    
prop_parseItems2 = monadicIO $ do
    res <- run $ testExample items2
    assert res
    
prop_parseWorld1 = monadicIO $ do
    res <- run $ testExample world1
    assert res


tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests


