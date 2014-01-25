module GameLogic.Language.Parser where

import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Prim hiding (token, tokens, parse)
import qualified Text.Parsec.Prim as P (parse)

import GameLogic.Data.Facade

data ResourceToken = IntResource String (Int, Int)
  deriving (Show, Read, Eq)

data Token = Comment String
           | Item String [ResourceToken]
  deriving (Show, Read, Eq)

symbols = "!@#$%^&*()`~-_=+[]{};'\\:\"|,./<>?"
quote = char '"'
underscore = char '_'

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
identation cnt = count cnt (space <|> tab)

--------------------------------------------------------

tokens :: GenParser Char st [Token]
tokens = many token

token :: GenParser Char st Token
token = comment <|> item <?> "token"

item :: GenParser Char st Token
item = try $ do
    string "Item" >> spaces
    itemName <- stringConstant
    rs <- resources
    return $ Item itemName rs

resources :: GenParser Char st [ResourceToken]
resources = try eol >> many resourceLine

resourceLine :: GenParser Char st ResourceToken
resourceLine = try (do r <- resource
                       many eol
                       return r) <?> "resourceLine"

resource :: GenParser Char st ResourceToken
resource = do
    identation 4
    name <- identifier
    spaces >> char '=' >> spaces
    val <- resourceValue
    return $ IntResource name val

resourceValue :: GenParser Char st (Int, Int)
resourceValue = do
    char '('
    v1 <- integerConstant
    spaces >> char ',' >> spaces
    v2 <- integerConstant
    char ')'
    return (v1, v2)

comment :: GenParser Char st Token
comment = try $ do
    char ';'
    str <- many (space <|> tab <|> alphaNum <|> oneOf symbols)
    return $ Comment str

------------------------------------------------------------

parseTokens :: String -> Either String [Token]
parseTokens input = case P.parse tokens [] input of
    Left err -> Left $ show err
    Right ts -> Right ts

parseToken :: String -> Either String Token
parseToken input = case P.parse token [] input of
    Left err -> Left $ show err
    Right token -> Right token
    
parseItem :: String -> Either String Token
parseItem input = case P.parse item [] input of
    Left err -> Left $ show err
    Right token -> Right token

parseResource :: String -> Either String ResourceToken
parseResource input = case P.parse resource [] input of
    Left err -> Left $ show err
    Right token -> Right token
    
parseResources :: String -> Either String [ResourceToken]
parseResources input = case P.parse resources [] input of
    Left err -> Left $ show err
    Right token -> Right token

parse = parseTokens
