module GameLogic.Language.Translating.Translator where

import GameLogic.Language.Parsing.RawParser as RP
import GameLogic.Language.RawToken as RT

import GameLogic.Language.Translating.Runtime
import GameLogic.Language.Translating.Rules

import Control.Monad.State
--import Control.Monad.Trans
import Control.Monad.Trans.Either (runEitherT)


nextId :: Int -> State TransRt Int
nextId prevId = do
    let nId = prevId + 1
    ctx <- get
    put $ ctx { trtNextId = nextId nId }
    return nId

indexingRt = initialRt (nextId 1)

translateToWorld [] = Left "There are no tokens." 
translateToWorld tokens = return $ evalState (runEitherT (translate scheme tokens)) indexingRt

translateToWorld' _ [] = Left "There are no tokens." 
translateToWorld' eF tokens = return $ eF (runEitherT (translate scheme tokens)) indexingRt

toWorld rawString = do
    ts <- RP.parseRawTokens rawString :: Either String [RawToken]
    translateToWorld' runState ts

    