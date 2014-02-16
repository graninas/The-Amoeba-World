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

-- Makes from scheme's list the list of context modificators.
-- Folds the new list with applying to current context.
apply sc t = sequence_ (map ($ t) sc)

{- Equal function:
apply' sc t = do
    ctx <- get
    let ctxModifier t ctx mod = do
        put ctx
        mod t
        get
    foldM_ (ctxModifier t) ctx sc
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
    translateToWorld' runState ts

    