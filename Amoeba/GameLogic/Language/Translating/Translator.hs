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

{-
; General items

Item "Karyon"
    lifebound = (0, 5000)
    durability = (100, 100)
    energy = (300, 2000)

; Conductor
Item "Conductor"
    lifebound = (0, 1000)
    durability = (100, 100)
    energy = (0, 100)

; World definition file

World "Pandora"
    width = 20
    height = 20
    defaultCell = Object "Empty" "Player0"
    cells =
        (10, 10): Object "Karyon" "Player1"
        (9, 9):   Object "Plasma" "Player1"
        (9, 10):  Object "Plasma" "Player1"
        (9, 11):  Object "Plasma" "Player1"
        (10, 9):  Object "Plasma" "Player1"
        (10, 11): Object "Plasma" "Player1"
        (11, 9):  Object "Plasma" "Player1"
        (11, 10): Object "Plasma" "Player1"
        (11, 11): Object "Plasma" "Player1"
        (15, 15): Object "Karyon" "Player2"
-}


nextId :: Int -> State TransRt Int
nextId prevId = do
    let nId = prevId + 1
    ctx <- get
    put $ ctx { trtNextId = nextId nId }
    return nId

indexingRt = initialRt (nextId 1)


apply sc t = mapM_ ($ t) sc

translate _ [] = return ()
translate sc (t:ts) = do
    apply sc t
    translate sc ts
        

translateToWorld [] = Left "There are no tokens." 
translateToWorld tokens = return $ execState (runEitherT (translate scheme tokens)) indexingRt

toWorld rawString = do
    ts <- RP.parseRawTokens rawString :: Either String [RawToken]
    ctx <- translateToWorld ts
    return $ trtLog ctx

    