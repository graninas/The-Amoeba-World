module GameLogic.Language.Parsing.ItemParser where

import GameLogic.Language.Parsing.Common
import GameLogic.Language.RawToken

import Middleware.Parsing.Facade as P

item :: GenParser Char st RawToken
item = do
    string "Item" >> many1 trueSpace
    itemName <- stringConstant
    lineEnd
    rs <- resources
    return $ Item itemName rs

resources :: GenParser Char st [PropertyToken]
resources = many resource

resource :: GenParser Char st PropertyToken
resource = do
    identation 4
    name <- identifier
    trueSpaces >> char '=' >> trueSpaces
    val <- resourceValue
    lineEnd
    return $ IntResource name val

resourceValue :: GenParser Char st (Int, Int)
resourceValue = intTuple2


