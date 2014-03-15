module GameLogic.Language.Translation.Actions where

import GameLogic.Language.RawToken
import GameLogic.Language.Translation.Runtime

import GameLogic.Data.World
import GameLogic.Data.Object

import Prelude hiding (log)
import Control.Monad.Trans.Either (left)
import Control.Monad (liftM)

-- Binds the trigger to the action
(/>) :: Show a => (a -> Bool) -> (a -> Trans ()) -> a -> Trans ()
(/>) trigger act token = if trigger token
                         then act token
                         else logExt $ "Token hasn't been triggered: " ++ show token
-- The same, but can return some result.
(<-/>) :: Show a => (a -> Bool) -> (a -> Trans b) -> a -> Trans (Maybe b)
(<-/>) trigger act token = if trigger token
                           then liftM Just $ act token
                           else do logExt $ "Token hasn't been triggered: " ++ show token
                                   return Nothing

-- Action that does nothing, only logs info.
skip :: Show a => a -> Trans ()
skip t =  log $ "Skip for: " ++ show t

-- Action inserts item as object template.

makeObjectTemplate = error "1"


getObjectTypeId _ = do
    prevTypeId <- getObjectTemplateMapSize
    return $ prevTypeId + 1

checkObjectTemplateUniqueness name = do
    mbObjectTemplate <- lookupObjectTemplate name
    case mbObjectTemplate of
        Nothing -> return ()
        Just _ -> left $ "Object template for item " ++ name ++ " is duplicated."

addObjectTemplate rules name props = do
    checkObjectTemplateUniqueness name
    objTypeId <- getObjectTypeId name
    
    objTemplate <- evaluate (makeObjectTemplate objTypeId) rules props
    
    insertObjectTemplate name objTemplate
    log $ "Object template for item " ++ name ++ " added."

addItem rules (ItemToken name props) = do
    log $ "Adding object template for: " ++ show name
    addObjectTemplate rules name props
addItem _ t = left $ "addItem: unexpected token got: " ++ show t

-- TODO: too special and too unobvious functions. Maybe I can better?
makeIntResource (IntResource name i) | isResourceValid i = return (name, toResource i)
makeIntResource (IntResource name i) = left $ "Invalid resource '" ++ name ++ "': " ++ show i
makeIntResource p = left $ "makeIntResource: unexpected property got: " ++ show p

setupWorld rules (WorldToken name props) = do
    log $ "Setting World: " ++ name ++ "."
    translate rules props
setupWorld _ t = left $ "setupWorld: unexpected token got: " ++ show t

-- TODO: use Lenses to update it.
setWidth (IntProperty _ i) = do
    log $ "setWidth: " ++ show i
    w <- getWorld
    let w' = w { width = i }
    setWorld w'
setWidth p = left $ "setWidth: unexpected property got: " ++ show p

-- TODO: use Lenses to update it.
setHeight (IntProperty _ i) = do
    log $ "setHeight" ++ show i
    w <- getWorld
    let w' = w { height = i }
    setWorld w'
setHeight p = left $ "setHeight: unexpected property got: " ++ show p

setCells (CellsProperty _ propTokens) = do
    log "setCells"
    mapM_ setCell propTokens
setCells p = left $ "setCells: unexpected property got: " ++ show p

setCell (CellProperty _ p@(x, y) (ObjectToken objName plName)) = do
    log $ "set cell: " ++ show p
    
setCell p = left $ "setCell: unexpected property got: " ++ show p