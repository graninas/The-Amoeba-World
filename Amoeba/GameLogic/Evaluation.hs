{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import Control.Monad
import Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, listToMaybe)
import Prelude hiding (read)

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.AI as AI
import GameLogic.GenericAI as GAI
import qualified GameLogic.GenericWorld as GW
import Misc.Descriptions

type TransactionMap = GW.GenericMap Object

data EvalError = ENoSuchProperty String
               | ENoActedObjectSet
               | ENotFound
               | EOverlappedObjects Objects
  deriving (Show, Read, Eq)

type EvalType ctx res = EitherT EvalError (State ctx) res
type Eval res = EvalType EvaluationContext res
type EvalState res = State EvaluationContext res

data DataContext = DataContext { _dataObjects :: Eval Objects
                               , _dataObjectGraph :: Eval ObjectGraph
                               , _dataObjectAt :: Point -> Eval (Maybe Object) }

data EvaluationContext = EvaluationContext { _ctxData :: DataContext
                                           , _ctxTransactionMap :: TransactionMap
                                           , _ctxActedObject :: Maybe Object
                                           , _ctxNextRndNum :: Eval Int }

makeLenses ''DataContext
makeLenses ''EvaluationContext

eNoSuchProperty = ENoSuchProperty
eNoActedObjectSet = ENoActedObjectSet
eNotFound = ENotFound
eOverlappedObjects = EOverlappedObjects
isEOverlappedObjects (EOverlappedObjects _) = True
isEOverlappedObjects _ = False
isENotFound ENotFound = True
isENotFound _ = False

-- context
noActedObject = Nothing

dataContext = DataContext
context dtCtx = EvaluationContext dtCtx GW.emptyMap noActedObject

nextRndNum :: Eval Int
nextRndNum = get >>= _ctxNextRndNum

-- TODO: replace by Graph
getObjectAt :: Point -> Eval (Maybe Object)
getObjectAt p = get >>= flip (_dataObjectAt . _ctxData) p

getObjects :: Eval Objects
getObjects = get >>= (_dataObjects . _ctxData)

getObjectGraph :: Eval ObjectGraph
getObjectGraph = get >>= (_dataObjectGraph . _ctxData)

ctxObjects     = ctxData . dataObjects
ctxObjectGraph = ctxData . dataObjectGraph
ctxObjectAt    = ctxData . dataObjectAt

-- querying

(~&~) p1 p2 obj = p1 obj && p2 obj

infixr 3 ~&~

isJustTrue :: Maybe Bool -> Bool
isJustTrue (Just x) = x
isJustTrue Nothing = False

maybeStored prop pred obj = let
    mbVal = obj ^? prop
    mbRes = liftM pred mbVal
    in if isJustTrue mbRes
            then mbVal
            else Nothing

is prop val = isJust . maybeStored prop (val ==) :: Object -> Bool
suchThat prop pred = isJust . maybeStored prop pred :: Object -> Bool
justAll :: Object -> Bool
justAll _ = True

filterObjects f = liftM (filter f) getObjects

query :: (Object -> Bool) -> Eval Objects
query q = do
    objs <- filterObjects q
    case objs of
        [] -> E.left eNotFound
        xs -> E.right xs

find :: (Object -> Bool) -> Eval (Maybe Object)
find q  = liftM listToMaybe (query q) :: Eval (Maybe Object)

single :: (Object -> Bool) -> Eval Object
single q = do
    found <- query q
    case found of
        []     -> E.left eNotFound
        (x:[]) -> E.right x
        xs     -> E.left $ eOverlappedObjects xs

-- evaluation

setupActedObject :: Object -> EvalState ()
setupActedObject obj = ctxActedObject .= Just obj
dropActedObject :: EvalState ()
dropActedObject = ctxActedObject .= Nothing
getActedObject :: Eval Object
getActedObject = do
    mbActedObject <- use ctxActedObject
    maybe (E.left eNoActedObjectSet) E.right mbActedObject

read prop = do
    obj <- getActedObject
    let er = E.left $ eNoSuchProperty $ nameProperty prop
    maybe er E.right (obj ^? prop)

save :: Object -> Eval ()
save obj = do
    let p = obj ^. singular objectDislocation
    ctxTransactionMap . at p .= Just obj

rollback :: TransactionMap -> EvalError -> EvalState String
rollback m err = do
    ctxTransactionMap .= m
    return $ "Transaction error: " ++ show err
commit :: String -> EvalState String
commit = return

transact :: Eval String -> Object -> EvalState String
transact act obj = do
        setupActedObject obj
        oldTransMap <- use ctxTransactionMap
        actRes <- eitherT (rollback oldTransMap) commit act
        dropActedObject
        return actRes

withProperty prop act = doWithProperty :: Eval [String]
  where
    doWithProperty = do
        objs <- filterObjects (has prop)
        evalTransact act objs

evaluatePlacementAlg PlaceToNearestEmptyCell obj = do
    let p = obj ^. singular objectDislocation
    objGraph <- use ctxObjectGraph
    
    
    
    return p -- TODO

evalTransact :: Eval String -> Objects -> Eval [String]
evalTransact act [] = return []
evalTransact act (o:os) = do
    ctx <- get
    let (res, newCtx) = runState (transact act o) ctx
    put newCtx
    ress <- evalTransact act os
    return (res : ress)

evaluate scenario = evalState (runEitherT scenario)
execute scenario = execState (runEitherT scenario)
run scenario = runState (runEitherT scenario)
