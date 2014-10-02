{-#  LANGUAGE MultiWayIf #-}
module Amoeba.GameLogic.Language.Parsing.WorldParser where

import Amoeba.GameLogic.Language.Parsing.Common
import Amoeba.GameLogic.Language.RawToken
import qualified Amoeba.GameLogic.Language.Scheme as S

import Amoeba.Middleware.Parsing.Facade as P

world :: GenParser Char st RawToken
world = do
    string S.world >> many1 trueSpace
    itemName <- stringConstant
    lineEnd
    rs <- properties
    return $ WorldToken itemName rs

properties :: GenParser Char st [PropertyToken]
properties = many property

property :: GenParser Char st PropertyToken
property = do
    identation 4
    name <- identifier
    if | name == S.width       -> intProperty name
       | name == S.height      -> intProperty name
       | name == S.defaultCell -> objectProperty name
       | name == S.cells       -> cellsProperty name
       | otherwise             -> fail $ "unknown property: " ++ name
       

{-
Uncomment this in case of GHC < 7.6 (no MultiWayIf support).

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
-}

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
    return $ CellsProperty name (concat cs)

cell :: GenParser Char st [PropertyToken]
cell = try singleCell <|> try multiCell <?> "cell"

multiCell :: GenParser Char st [PropertyToken]
multiCell = do
    identation 8
    o@(ObjectToken oName plName) <- object
    trueSpaces
    coords <- listOf intTuple2
    lineEnd
    return $ map (\c -> CellProperty S.cell c o) coords

singleCell :: GenParser Char st [PropertyToken]
singleCell = do
    identation 8
    coords <- intTuple2
    trueSpaces >> char ':' >> trueSpaces
    o <- object
    lineEnd
    return $ [CellProperty S.cell coords o]
