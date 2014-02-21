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
import qualified GameLogic.Language.Scheme as S

-- 'ARF' stands for 'Amoeba Raw File' or 'Amoeba Raw Format' if you wish.

items1 = ("Items1", "./Data/Raws/Items.arf",
         parseRawTokens,
         Right [Comment " General items",EmptyToken,Item S.karyon [IntResource S.lifebound (0,5000),IntResource S.durability (100,100),IntResource S.energy (300,2000)],EmptyToken,Comment " Conductor",Item S.conductor [IntResource S.lifebound (0,1000),IntResource S.durability (100,100),IntResource S.energy (0,100)]])
items2 = ("Items2", "./Data/Raws/Item.arf",
         parseRawTokens,
         Right [Item S.karyon [IntResource S.lifebound (0,5000), IntResource S.durability (100,100), IntResource S.energy (300,2000)]])
world1 = ("World1", "./Data/Raws/World1.arf",
         parseRawTokens,
         Right [ Comment " World definition file"
               , EmptyToken
               , World "Pandora" [ IntProperty S.width 20, IntProperty S.height 20, ObjectProperty S.defaultCell (Object S.empty S.player0)
                                 , CellsProperty S.cells [ CellProperty S.cell (10, 10) (Object S.karyon S.player1)
                                                         , CellProperty S.cell (9, 9) (Object S.plasma S.player1)]] ])
world2 = ( "World2"
         , "./Data/Raws/World2.arf"
         , parseRawTokens
         , undefined )
world3 = ( "World3"
         , "./Data/Raws/World3.arf"
         , parseRawTokens
         , undefined )

parseExample parser dataFile = liftM parser (readFile dataFile)

testExample ex@(testName, dataFile, parser, res) = do
    parsed <- parseExample parser dataFile
    return $ res == parsed

examineExample ex@(testName, dataFile, parser, res) pred = do
    parsed <- parseExample parser dataFile
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
    p <- parseExample parseRawTokens rawFile
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
    