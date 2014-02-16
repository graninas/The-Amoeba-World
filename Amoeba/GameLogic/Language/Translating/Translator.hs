module GameLogic.Language.Translating.Translator where

import GameLogic.Language.Parsing.RawParser as RP
import GameLogic.Language.RawToken as RT

import qualified GameLogic.Runtime.World as W
import qualified GameLogic.Data.Facade as D

import GameLogic.Language.Translating.Scheme
import GameLogic.Language.Translating.Runtime

import Prelude hiding (log)

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import Control.Monad
import Data.Either.Combinators (isRight)


nextId :: Int -> State TransRt Int
nextId prevId = do
    let nId = prevId + 1
    ctx <- get
    put $ ctx { trtNextId = nextId nId }
    return nId

indexingRt = initialRt (nextId 1)


apply sc t = sequence_ (map ($ t) sc)
    {-
    let acts = [act | act <- map ($ t) sc]
    case acts of
        [] -> left $ "No translator for token " ++ show t
        (act : []) -> act
        _ -> left $ "Redundant translators for token " ++ show t
    -}
    
translate _ [] = return ()
translate sc (t:ts) = do
    apply sc t
    translate sc ts


translateToWorld [] = Left "There are no tokens." 
translateToWorld tokens = return $ evalState (runEitherT (translate scheme tokens)) indexingRt

translateToWorld' _ [] = Left "There are no tokens." 
translateToWorld' eF tokens = return $ eF (runEitherT (translate scheme tokens)) indexingRt


toWorld rawString = do
    ts <- RP.parseRawTokens rawString :: Either String [RawToken]
    res <- translateToWorld' execState ts
    return $ trtItemMap res

    