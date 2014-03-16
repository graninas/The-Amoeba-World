module GameLogic.Language.Translation.Runtime where

import Control.Monad (when, liftM)
import Control.Monad.State
import Control.Monad.Trans.Either as E
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Prelude hiding (log)

import GameLogic.Data.Types
import GameLogic.Data.World
import GameLogic.Data.Object
import GameLogic.Language.RawToken

{- Translation Runtime.
Takes translation rules and RawToken tree and returns complete World object.
Rules == tree-like translation scheme consisting of a triggers binded to an actions.

For now, there is an overhead: the 'trigger->action bind operator' looks through full current list of rules
and selects an appropriate rule by name. It could be improved to select that rule from map.
Or even list/map could be modified to a plain structure, so 'selecting' might be thrown out.

Also, it is possible to merge and unify RawToken and PropertyToken.
-}


type ObjectTemplate = (ObjectType, [PropertyToken])
type ObjectTemplateMap = M.Map String ObjectTemplate

data TransRt = TransRt { trtNextId :: State TransRt Int
                       , trtObjectTemplateMap :: ObjectTemplateMap
                       , trtWorld :: World
                       , trtLog :: [String]
                       , trtExtendedLogs :: Bool
                       }

type Trans a = EitherT String (State TransRt) a

-- System
getExtendedLogs :: Trans Bool
getExtendedLogs = liftM trtExtendedLogs get

getWorld :: Trans World
getWorld = liftM trtWorld get

-- TODO: use Lenses to update it.
setWorld w = do
    ctx <- get
    let ctx' = ctx { trtWorld = w }
    put ctx'

-- TODO: use Lenses to update it.
log s = do
    ctx <- get
    let newCtx = ctx { trtLog = trtLog ctx ++ [s] }
    put newCtx

logExt s = do
    isExtendedLogs <- getExtendedLogs
    when isExtendedLogs (log s)
    
initialRt idCounter = TransRt idCounter M.empty emptyWorld [] False

-- Makes from rules list the list of context modifiers.
-- Folds the new list with applying to current context.
apply_ rules token = mapM_ ($ token) rules

-- translate :: rules -> [RawToken] -> Trans ()
translate rules = mapM_ (apply_ rules)

-- Specific
-- TODO: use Lenses to update it.
getObjectTemplateMap :: Trans ObjectTemplateMap
getObjectTemplateMap = liftM trtObjectTemplateMap get

getObjectTemplateMapSize = liftM M.size getObjectTemplateMap

putObjectTemplateMap m = do
    ctx <- get
    put $ ctx { trtObjectTemplateMap = m }

insertObjectTemplate name objTemplate = do
    m <- getObjectTemplateMap
    putObjectTemplateMap $ M.insert name objTemplate m

lookupObjectTemplate name = liftM (M.lookup name) getObjectTemplateMap

