module GameLogic.Language.Parser where

import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Prim  hiding (token)

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

items :: GenParser Char st [Object]
items = undefined

token :: GenParser Char st Token
token = comment <|> item <?> "token"

item :: GenParser Char st Token
item = try $ do
    string "Item" >> spaces
    itemName <- stringConstant
    rs <- resources
    return $ Item itemName rs

resources :: GenParser Char st [ResourceToken]
resources = many eol >> many resourceLine

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

parseToken :: String -> Either String Token
parseToken input = case parse token [] input of
    Left err -> Left $ show err
    Right token -> Right token
    
parseItem :: String -> Either String Token
parseItem input = case parse item [] input of
    Left err -> Left $ show err
    Right token -> Right token

parseResource :: String -> Either String ResourceToken
parseResource input = case parse resource [] input of
    Left err -> Left $ show err
    Right token -> Right token
    
parseResources :: String -> Either String [ResourceToken]
parseResources input = case parse resources [] input of
    Left err -> Left $ show err
    Right token -> Right token

--parse :: String -> Either [Objects]

{-
; General items

Item "Karyon"
    lifebound = (0, 5000)
    durabilirty = (100, 100)
    energy = (300, 2000)

; Conductor
Item "Conductor"
    lifebound = (0, 1000)
    durability = (100, 100)
    energy = (0, 100)


parse :: String -> IO Dsl
parse input = do
    let parsed = Parsec.parse parseDslTokens [] input
    case parsed of
        Left err -> putStrLn ("PARSE ERROR\n\n" ++ show err) >> fail ""
        Right (Dsl tokens) -> return . Dsl $ tokens

parseDslTokens :: GenParser Char st Dsl
parseDslTokens = do
    res <- many dslTokenLine
    return (Dsl res)

dslTokenLine :: GenParser Char st DslToken
dslTokenLine = try emptyDslToken <|> dslToken <?> "dslTokenDefinerLine"

emptyDslToken :: GenParser Char st DslToken
emptyDslToken = eol >> return EmptyDslToken

dslToken :: GenParser Char st DslToken
dslToken = do
    tokenName               <- dslTokenName
    tokenDescriptor         <- dslTokenDescription
    (tokenBody, tokenType)  <- dslTokenBody
    -- emptyStringGuard -- TODO
    return (DslToken tokenType tokenName tokenDescriptor tokenBody)
    
dslTokenDescription :: GenParser Char st DslTokenDescription
dslTokenDescription = do
    spaces
    try fullDslTokenDescriptor <|> emptyDslTokenDescriptor <?> "dslTokenDescription"

fullDslTokenDescriptor :: GenParser Char st DslTokenDescription
fullDslTokenDescriptor = do
    char ':' >> spaces
    res <- stringConstant
    spaces >> char '='
    return (DslTokenDescription (Just res))

consumeSpaces :: Bool -> GenParser Char st ()
consumeSpaces True = space >> spaces
consumeSpaces False = spaces

consumeChar :: Char -> Bool -> GenParser Char st ()
consumeChar _ False = return ()
consumeChar ch True = try (char ch >> return ()) <|> error ("consumeChar: " ++ [ch])

lookForChar :: Char -> GenParser Char st Bool
lookForChar ch = try (lookAhead (char ch) >> return True) <|> return False

emptyList :: GenParser Char st ()
emptyList = char '[' >> spaces >> char ']' >> return ()

fullAdtIdentifier :: String -> GenParser Char st String
fullAdtIdentifier s | (not $ null s) = do
    c <- upper
    cs <- many (alphaNum <|> char '_')
    let fullId = (c : cs)
    if fullId == s
        then return s
        else fail "Identifier does not recognized"
fullAdtIdentifier s | otherwise = fail "Empty identifier"
-}