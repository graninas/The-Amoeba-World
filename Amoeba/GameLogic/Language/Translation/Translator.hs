module GameLogic.Language.Translation.Translator where

import GameLogic.Language.Parsing.RawParser as RP
import GameLogic.Language.RawToken as RT
import GameLogic.Language.Translation.Runtime
import GameLogic.Language.Translation.Rules
import GameLogic.Data.World

import Control.Monad.State
import Control.Monad.Trans.Either (runEitherT)


nextId :: Int -> Trans Int
nextId prevId = do
    let nId = prevId + 1
    ctx <- get
    put $ ctx { trtNextId = nextId nId }
    return nId

indexingRt = initialRt (nextId 1)

translateToWorld stateFunc [] = Left "There are no tokens." 
translateToWorld stateFunc tokens = return $ stateFunc (runEitherT (translate scheme tokens)) indexingRt

toWorld :: String -> Either String World
toWorld rawString = do
    tokens <- RP.parseRawTokens rawString :: Either String [RawToken]
    translateRuntime <- translateToWorld execState tokens
    return $ trtWorld translateRuntime

-- For test purposes.

toWorld' stateFunc rawString = do
    tokens <- RP.parseRawTokens rawString :: Either String [RawToken]
    translateToWorld stateFunc tokens
    