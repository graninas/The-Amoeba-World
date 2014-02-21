module GameLogic.Language.Translating.Actions where

import GameLogic.Language.RawToken
import GameLogic.Language.Translating.Runtime

import GameLogic.Data.World

import Prelude hiding (log)
import Control.Monad.Trans.Either (left)

-- Binds trigger to an action
(/>) :: Show a => (a -> Bool) -> (a -> Trans ()) -> a -> Trans ()
(/>) trigger act token = if trigger token
                         then act token
                         else logExt $ "Token hasn't been triggered: " ++ show token

-- Action that do nothing, only logs info.
skip :: Show a => a -> Trans ()
skip t =  log $ "Skip for: " ++ show t

-- Action inserts item as object template.
addItem (ItemToken name props) = do
    log $ "Adding object template for: " ++ show name
    insertObjectTemplate name props
addItem t = left $ "addItem: unexpected token got: " ++ show t

setupWorld rules (WorldToken name props) = do
    log $ "Setting World: " ++ name ++ "."
    translate rules props
setupWorld _ t = left $ "setupWorld: unexpected token got: " ++ show t

setWidth (IntProperty _ i) = do
    log "setWidth"
--    w <- getWorld
--    setWorld $ w { 
    
setWidth p = left $ "setWidth: unexpected property got: " ++ show p

setHeight (IntProperty _ i) = log $ "setHeight"
setHeight p = left $ "setHeight: unexpected property got: " ++ show p

setCells _ = log $ "setCells"
