module GameLogic.Language.Translation.Actions where

import GameLogic.Language.RawToken
import GameLogic.Language.Translation.Runtime
import qualified GameLogic.Language.Scheme as Scheme

import GameLogic.Data.World
import GameLogic.Data.Object
import GameLogic.Data.Types

import Prelude hiding (log)
import Control.Monad.Trans.Either (left)
import Control.Monad (liftM)
import qualified Data.Map as M

-- Binds the trigger to the action
(/>) :: Show a => (a -> Bool) -> (a -> Trans ()) -> a -> Trans ()
(/>) trigger act token = if trigger token
                         then act token
                         else logExt $ "Token hasn't been triggered: " ++ show token

-- Action that does nothing, only logs info.
skip :: Show a => a -> Trans ()
skip t =  log $ "Skip for: " ++ show t

getObjectType _ = do
    prevType <- getObjectTemplateMapSize
    return $ prevType + 1

checkObjectTemplateUniqueness name = do
    mbObjectTemplate <- lookupObjectTemplate name
    case mbObjectTemplate of
        Nothing -> return ()
        Just _ -> left $ "Object template for item " ++ name ++ " is duplicated."

addObjectTemplate :: String -> [PropertyToken] -> Trans ()
addObjectTemplate name props = do
    checkObjectTemplateUniqueness name
    objType <- getObjectType name
    insertObjectTemplate name (objType, props)
    log $ "Object template for item " ++ name ++ " added."

addItem (ItemToken name props) = do
    log $ "Adding object template for: " ++ show name
    addObjectTemplate name props
addItem t = left $ "addItem: unexpected token got: " ++ show t

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

setCell (CellProperty _ p obj) = do
    log $ "set cell: " ++ show p
    addObject p obj
setCell p = left $ "setCell: unexpected property got: " ++ show p

getObjectTemplate name = do
    mbObjTemplate <- lookupObjectTemplate name
    case mbObjTemplate of
        Just x -> return x
        Nothing -> left $ "Object template for object " ++ name ++ " not found."

-- TODO: too special and too unobvious functions. Maybe I can better?
addObject (x, y) (ObjectToken name plName) = do
    (objType, props) <- getObjectTemplate name
    intResourceMap <- translateIntResourceTokens props
    energyResource <- getResource Scheme.energy intResourceMap
    durabilityResource <- getResource Scheme.durability intResourceMap
    lifeboundResource <- getResource Scheme.lifebound intResourceMap
    return ()

getResource rName intResourceMap = case M.lookup rName intResourceMap of
    Nothing -> left $ "Resource with name " ++ rName ++ " not found."
    Just r -> return r

makeIntResource (IntResourceProperty name i) | isResourceValid i = return (name, toResource i)
makeIntResource (IntResourceProperty name i) = left $ "Invalid resource '" ++ name ++ "': " ++ show i
makeIntResource p = left $ "makeIntResource: unexpected property got: " ++ show p

translateIntResourceTokens props = do
    intResources <- mapM makeIntResource props
    return $ foldr (uncurry M.insert) M.empty intResources

