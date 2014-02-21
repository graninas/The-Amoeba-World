module GameLogic.Language.Translating.Actions where

import GameLogic.Language.RawToken
import GameLogic.Language.Translating.Runtime

import Prelude hiding (log)
import Control.Monad.Trans.Either (left)

(/>) :: Show a => (a -> Bool) -> (a -> Trans ()) -> a -> Trans ()
(/>) trigger act token = if trigger token
                         then act token
                         else logExt $ "Token not triggered: " ++ show token


skip :: Show a => a -> Trans ()
skip t =  log $ "Skip for: " ++ show t

addItem (Item name props) = do
    log $ "Adding object template for: " ++ show name
    insertObjectTemplate name props
addItem t = left $ "addItem: Item expected but got " ++ show t

{-
setupWorld (World name props) = do
    log "Setting World."
    wh <- get int "width" props
    ht <- get int "height" props
    cells <- getWorldCells props
    constructWorld wg ht cells
setupWorld t = left $ "setupWorld: World expected but got " ++ show t

getIntProperty :: String -> PropertyToken -> Trans Int
getIntProperty name (IntProperty n i : ps) | name == n = return i
getIntProperty name _ = 

getWorldCells props = do
-}

--setupWorld :: a -> PropertyToken -> Trans ()
setupWorld propsScheme t = undefined

setWidth = undefined
setHeight = undefined
setCells = undefined
