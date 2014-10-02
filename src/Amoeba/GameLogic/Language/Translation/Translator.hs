module Amoeba.GameLogic.Language.Translation.Translator
    (parseWorld, parseWorld') where

import Amoeba.GameLogic.Language.Parsing.RawParser as RP
import Amoeba.GameLogic.Language.RawToken as RT
import Amoeba.GameLogic.Language.Translation.Runtime
import Amoeba.GameLogic.Language.Translation.Rules
import Amoeba.GameLogic.Data.World

import Control.Monad.State
import Control.Monad.Trans.Either (runEitherT)


nextId :: Int -> Trans Int
nextId prevId = do
    let nId = prevId + 1
    ctx <- get
    put $ ctx { trtNextId = nextId nId }
    return nId

indexingRt = initialRt (nextId 1)

translate' stateFunc [] = Left "There are no tokens." 
translate' stateFunc tokens = return $ stateFunc (runEitherT (translate scheme tokens)) indexingRt

parseWorld :: String -> Either String World
parseWorld rawString = do
    tokens <- RP.parseRawTokens rawString :: Either String [RawToken]
    translateRuntime <- translate' execState tokens
    return $ trtWorld translateRuntime
  
-- For test purposes.

parseWorld' stateFunc rawString = do
    tokens <- RP.parseRawTokens rawString :: Either String [RawToken]
    translate' stateFunc tokens
    