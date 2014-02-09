module GameLogic.Language.Parsers.WorldParser where

import GameLogic.Language.Parsers.RawToken
import GameLogic.Language.Parsers.Common

import Middleware.Parsing.Facade as P

world :: GenParser Char st RawToken
world = do
    string "World" >> many1 trueSpace
    itemName <- stringConstant
    lineEnd
    rs <- properties
    return $ World itemName rs

properties :: GenParser Char st [PropertyToken]
properties = many property

property :: GenParser Char st PropertyToken
property = do
    identation 4
    name <- identifier
    case name of
        "width" -> intProperty name
        "height" -> intProperty name
        "defaultCell" -> objectProperty name
        "cells" -> cellsProperty name

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
    string "Object" >> many1 trueSpace
    objectName <- stringConstant
    many trueSpace
    playerName <- stringConstant
    return $ Object objectName playerName

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
    return $ CellProperty coords o
