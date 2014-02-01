module GameLogic.Language.Parsers.ItemParser where

import Middleware.Parsing.Facade as P
import GameLogic.Data.Facade

data ResourceToken = IntResource String (Int, Int)
  deriving (Show, Read, Eq)

data ItemToken = Comment String
               | Item String [ResourceToken]
               | EmptyToken
  deriving (Show, Read, Eq)

itemTokens :: GenParser Char st [ItemToken]
itemTokens = many itemToken

itemToken :: GenParser Char st ItemToken
itemToken = emptyToken <|> comment <|> item <?> "token"

emptyToken :: GenParser Char st ItemToken
emptyToken = eol >> return EmptyToken

comment :: GenParser Char st ItemToken
comment = do
    char ';'
    str <- many (noneOf "\n\r")
    lineEnd
    return $ Comment str

item :: GenParser Char st ItemToken
item = do
    string "Item" >> many1 trueSpace
    itemName <- stringConstant
    lineEnd
    rs <- resources
    return $ Item itemName rs

resources :: GenParser Char st [ResourceToken]
resources = many resource

resource :: GenParser Char st ResourceToken
resource = do
    identation 4
    name <- identifier
    trueSpaces >> char '=' >> trueSpaces
    val <- resourceValue
    lineEnd
    return $ IntResource name val

resourceValue :: GenParser Char st (Int, Int)
resourceValue = do
    char '('
    v1 <- integerConstant
    trueSpaces >> char ',' >> trueSpaces
    v2 <- integerConstant
    char ')'
    return (v1, v2)

------------------------------------------------------------

parseItemTokens :: String -> Either String [ItemToken]
parseItemTokens input = case P.parse itemTokens [] input of
    Left err -> Left $ show err
    Right ts -> Right ts

