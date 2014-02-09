module GameLogic.Language.Parsers.RawParser where

import GameLogic.Language.Parsers.RawToken
import GameLogic.Language.Parsers.WorldParser
import GameLogic.Language.Parsers.ItemParser
import GameLogic.Language.Parsers.Common

import Middleware.Parsing.Facade as P

rawTokens :: GenParser Char st [RawToken]
rawTokens = many rawToken

rawToken :: GenParser Char st RawToken
rawToken = emptyToken <|> comment <|> item <|> world <?> "rawToken"

parseRawTokens :: String -> Either String [RawToken]
parseRawTokens input = case P.parse rawTokens [] input of
    Left err -> Left $ show err
    Right ts -> Right ts
