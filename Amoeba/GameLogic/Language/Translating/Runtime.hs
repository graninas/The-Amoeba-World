module GameLogic.Language.Translating.Runtime where

import Control.Monad (when, liftM)
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import qualified Data.Map as M
import Prelude hiding (log)

import GameLogic.Data.Types
import GameLogic.Language.RawToken
import GameLogic.Runtime.World

type ObjectTemplate = (ObjectType, [PropertyToken])

data TransRt = TransRt { trtNextId :: State TransRt Int
                       , trtItemMap :: M.Map String ObjectTemplate
                       , trtWorldConstructor :: String --State TransRt World
                       , trtLog :: [String]
                       , trtExtendedLogs :: Bool
                       }

type Trans a = EitherT String (State TransRt) a

-- System

getExtendedLogs :: Trans Bool
getExtendedLogs = liftM trtExtendedLogs get

log s = do
    ctx <- get
    let newCtx = ctx { trtLog = trtLog ctx ++ [s] }
    put newCtx

logExt s = do
    isExtendedLogs <- getExtendedLogs
    when isExtendedLogs (log s)

initialRt idCounter = TransRt idCounter M.empty "Empty" [] False

-- Specific

getItemMap = liftM trtItemMap get
putItemMap m = do
    ctx <- get
    put $ ctx { trtItemMap = m }

objectTemplate objType props = (objType, props)

insertObjectTemplate :: String -> [PropertyToken] -> Trans ()
insertObjectTemplate name props = do
    m <- getItemMap
    let objectType = M.size m + 1
    case M.lookup name m of
        Nothing -> putItemMap $ M.insert name (objectTemplate objectType props) m
        Just _ -> left $ "Object template for item " ++ name ++ " is duplicated."
    log $ "Object template for item " ++ name ++ " added."

-- Makes from rules list the list of context modificators.
-- Folds the new list with applying to current context.
apply rules t = mapM_ ($ t) rules

{- Same function:
apply rules t = sequence_ (map ($ t) rules)
-}

{- Same function:
apply rules t = do
    ctx <- get
    let ctxModifier t ctx mod = do
        put ctx
        mod t
        get
    foldM_ (ctxModifier t) ctx rules
-}

translate _ [] = return ()
translate rules (t:ts) = do
    apply rules t
    translate rules ts