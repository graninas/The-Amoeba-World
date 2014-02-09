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

commentString :: GenParser Char st String
commentString = do
    char ';'
    str <- many (noneOf "\n\r")
    lineEnd
    return str

assignment = trueSpaces >> char '=' >> trueSpaces

intTuple2 :: GenParser Char st (Int, Int)
intTuple2 = do
    char '('
    v1 <- integerConstant
    trueSpaces >> char ',' >> trueSpaces
    v2 <- integerConstant
    char ')'
    return (v1, v2)