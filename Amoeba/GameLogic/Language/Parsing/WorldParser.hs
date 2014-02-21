module GameLogic.Language.Parsing.WorldParser where

import GameLogic.Language.Parsing.Common
import GameLogic.Language.RawToken
import qualified GameLogic.Language.Scheme as S

import Middleware.Parsing.Facade as P

world :: GenParser Char st RawToken
world = do
    string S.world >> many1 trueSpace
    itemName <- stringConstant
    lineEnd
    rs <- properties
    return $ WorldToken itemName rs

properties :: GenParser Char st [PropertyToken]
properties = many property

{- Uncomment for GHC 7.6.
 LANGUAGE MultiWayIf
property :: GenParser Char st PropertyToken
property = do
    identation 4
    name <- identifier
    if | name == width       -> intProperty name
       | name == height      -> intProperty name
       | name == defaultCell -> objectProperty name
       | name == cells       -> cellsProperty name
       | otherwise           -> fail $ "unknown property: " ++ name
       
-}

property :: GenParser Char st PropertyToken
property = do
    identation 4
    name <- identifier
    chooseProperty name
  where
        chooseProperty name | name == S.width       = intProperty name
                            | name == S.height      = intProperty name
                            | name == S.defaultCell = objectProperty name
                            | name == S.cells       = cellsProperty name
                            | otherwise             = fail $ "unknown property: " ++ name

intProperty :: String -> GenParser Char st PropertyToken
intProperty name = do
    assignment
    val <- integerConstant
    lineEnd
    return $ IntProperty name val

objectProperty :: String -> GenParser Char st PropertyToken
objectProperty name = do
    assignment
    o <- object
    lineEnd
    return $ ObjectProperty name o

object :: GenParser Char st RawToken
object = do
    string S.object >> many1 trueSpace
    objectName <- stringConstant
    many trueSpace
    playerName <- stringConstant
    return $ ObjectToken objectName playerName

cellsProperty :: String -> GenParser Char st PropertyToken
cellsProperty name = do
    cs <- assignment >> eol >> many1 cell
    return $ CellsProperty name cs

cell :: GenParser Char st PropertyToken
cell = do
    identation 8
    coords <- intTuple2
    trueSpaces >> char ':' >> trueSpaces
    o <- object
    lineEnd
    return $ CellProperty S.cell coords o
