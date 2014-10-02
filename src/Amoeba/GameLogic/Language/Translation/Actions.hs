module Amoeba.GameLogic.Language.Translation.Actions where

import Amoeba.GameLogic.Language.RawToken
import Amoeba.GameLogic.Language.Translation.Runtime

import Amoeba.GameLogic.Data.World
import Amoeba.GameLogic.Data.Object
import Amoeba.GameLogic.Data.Player
import Amoeba.Middleware.Math.Geometry (point)

import Prelude hiding (log)
import Control.Monad.Trans.Either (left)
import Control.Monad (liftM)
import qualified Data.Map as M

-- This imports are for hacks.
import qualified Amoeba.GameLogic.Language.Scheme as Scheme

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
-- TODO: hacks. I'd like to finish it right now.

playersMap = M.fromList [ (Scheme.player0, humanPlayer)
                        , (Scheme.player1, ai1Player) ]

toWordlPoint (x, y) = point x y 0

addObject p (ObjectToken name plName) = do
    (objType, props) <- getObjectTemplate name
    intResourceMap <- translateIntResourceTokens props
    energyResource <- getResource Scheme.energy intResourceMap
    durabilityResource <- getResource Scheme.durability intResourceMap
    lifeboundResource <- getResource Scheme.lifebound intResourceMap
    player <- getPlayer plName playersMap
    objId <- getNextId
    let object = Object objId objType player lifeboundResource durabilityResource energyResource
    insertWorldObject (toWordlPoint p) object

getPlayer name m = case M.lookup name m of
    Nothing -> left $ "Player with name " ++ name ++ " not found."
    Just p -> return p
getResource name m = case M.lookup name m of
    Nothing -> left $ "Resource with name " ++ name ++ " not found."
    Just r -> return r

makeIntResource (IntResourceProperty name i) | isResourceValid i = return (name, toResource i)
makeIntResource (IntResourceProperty name i) = left $ "Invalid resource '" ++ name ++ "': " ++ show i
makeIntResource p = left $ "makeIntResource: unexpected property got: " ++ show p

translateIntResourceTokens props = do
    intResources <- mapM makeIntResource props
    return $ M.fromList intResources

