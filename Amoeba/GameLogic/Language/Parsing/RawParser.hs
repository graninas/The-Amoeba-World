module GameLogic.Language.Parsing.RawParser where

import GameLogic.Language.Parsing.WorldParser
import GameLogic.Language.Parsing.ItemParser
import GameLogic.Language.Parsing.Common

import GameLogic.Language.RawToken

import Middleware.Parsing.Facade as P

rawTokens :: GenParser Char st [RawToken]
rawTokens = many rawToken

rawToken :: GenParser Char st RawToken
rawToken = try emptyToken <|> try comment <|> try item <|> try world <?> "rawToken"

parseRawTokens :: String -> Either String [RawToken]
parseRawTokens input = case P.parse rawTokens [] input of
    Left err -> Left $ show err
    Right ts -> Right ts
