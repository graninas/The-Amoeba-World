module GameLogic.Language.Parsers.WorldParser where

import Middleware.Parsing.Facade as P
import GameLogic.Parsers.Common

import Control.Monad (liftM)

import GameLogic.Data.Facade

data PropertyToken = IntProperty String Int
                   | ObjectProperty String
                   | CellsProperty
  deriving (Show, Read, Eq)

data WorldToken = Comment String
               | World String [PropertyToken]
               | EmptyToken
  deriving (Show, Read, Eq)

worldTokens :: GenParser Char st [WorldToken]
worldTokens = many worldToken

worldToken :: GenParser Char st WorldToken
worldToken = emptyToken <|> comment <|> item <?> "worldToken"

emptyToken :: GenParser Char st WorldToken
emptyToken = eol >> return EmptyToken

comment :: GenParser Char st WorldToken
comment = liftM Comment commentString

world :: GenParser Char st WorldToken
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

intProperty name = do
    assignment
    val <- integerConstant
    lineEnd
    return $ IntProperty name val

objectProperty name = do
    assignment
    o <- object
    


; World definition file

World "Pandora"
    width = 20
    height = 20
    defaultCell = Object "Empty"
    cells =
        (10, 10): Object "Karyon" "Player1"
        (9, 9):   Object "Plasma" "Player1"
        (9, 10):  Object "Plasma" "Player1"
        (9, 11):  Object "Plasma" "Player1"
        (10, 9):  Object "Plasma" "Player1"
        (10, 11): Object "Plasma" "Player1"
        (11, 9):  Object "Plasma" "Player1"
        (11, 10): Object "Plasma" "Player1"
        (11, 11): Object "Plasma" "Player1"
        (15, 15): Object "Karyon" "Player2"