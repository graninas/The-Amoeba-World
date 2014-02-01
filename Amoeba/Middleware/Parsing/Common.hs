module Middleware.Parsing.Common where

import Middleware.Parsing.ParsecFacade

symbols = "!@#$%^&*()`~-_=+[]{};'\\:\"|,./<>?"
quote = char '"'
underscore = char '_'
trueSpace = char ' ' <|> tab
trueSpaces = many trueSpace

stringConstant :: GenParser Char st String
stringConstant = between quote quote (many alphaNum)

integerConstant :: GenParser Char st Int
integerConstant = do
    res <- many1 digit
    return (read res)

identifier :: GenParser Char st String
identifier = do
    c <- lower <|> underscore
    rest <- many (alphaNum <|> underscore)
    return (c : rest)

eol :: GenParser Char st String
eol = try (string "\n\r") <|> try (string "\r\n") <|> try (string "\n") <|> try (string "\r") <?> "eol"

identation :: Int -> GenParser Char st String
identation cnt = count cnt trueSpace

lineEnd = trueSpaces >> optional eol