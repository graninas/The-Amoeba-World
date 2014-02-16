module GameLogic.Language.Translator where

import GameLogic.Language.Parsers.RawParser as RP
import GameLogic.Language.Parsers.RawToken as RT
import qualified GameLogic.Runtime.World as W
import qualified GameLogic.Data.Facade as D

import qualified Data.Map as M
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

data TransRt = TransRt { trtNextId :: State TransRt Int
                       , trtItemMap :: M.Map String String -- TODO
                       , trtWorldConstructor :: String -- TODO
                       , trtLog :: [String]
                       }

type Trans a = EitherT String (State TransRt) a

nextId :: Int -> State TransRt Int
nextId prevId = do
    let nId = prevId + 1
    ctx <- get
    put $ ctx { trtNextId = nextId nId }
    return nId



initialRt = TransRt (nextId 1) M.empty "Empty" []



(/>) trigger act = \token -> if trigger token
                             then act token
                             else return $ "Token not triggered: " ++ show token

log s = do
    ctx <- get
    let newCtx = ctx { trtLog = (s : trtLog ctx) }
    put newCtx

-- actions:
skip, addItem :: RawToken -> Trans String
skip t = do
    log $ "Skip for: " ++ show t
    return ""
addItem t = do
    log $ "Adding object template for: " ++ show t
    return ""

-- triggers:
onComment (RT.Comment _) = True
onComment _ = False
onEmpty RT.EmptyToken = True
onEmpty _ = False
onItem (Item n props) = True
onItem _ = False

scheme = [ onComment /> skip
         , onEmpty /> skip
         , onItem /> addItem
         ]

apply sc t = mapM_ ($ t) sc

translate _ [] = return ()
translate sc (t:ts) = do
    apply sc t
    translate sc ts
        

translateToWorld [] = Left "There are no tokens." 
translateToWorld tokens = return $ execState (runEitherT (translate scheme tokens)) initialRt

toWorld rawString = do
    ts <- RP.parseRawTokens rawString :: Either String [RawToken]
    ctx <- translateToWorld ts
    return $ trtLog ctx

    