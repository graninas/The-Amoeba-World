{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import Control.Monad
import Control.Lens
import Control.Applicative
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Monoid
import Data.Maybe (fromJust, isJust, listToMaybe)
import Prelude hiding (read)

import GameLogic.Geometry
import GameLogic.Object
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

data EvaluationContext = EvaluationContext { _ctxTransactionMap :: TransactionMap
                                           , _ctxActedObject :: Maybe Object
                                           , _ctxNextRndNum :: Eval Int
                                           , _ctxObjectAt :: Point -> Eval (Maybe Object)
                                           , _ctxObjects :: Eval Objects
                                           }

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

context = EvaluationContext GW.emptyMap noActedObject

nextRndNum :: Eval Int
nextRndNum = get >>= _ctxNextRndNum

objectAt :: Point -> Eval (Maybe Object)
objectAt p = get >>= flip _ctxObjectAt p

objects :: Eval Objects
objects = get >>= _ctxObjects

having prop = liftM (filter (has prop)) objects

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

query :: (Object -> Bool) -> Eval Objects
query q = do
    objs <- liftM (filter q) objects
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

read prop = use ctxActedObject >>= checkPropertyExist
  where
    checkPropertyExist Nothing = E.left eNoActedObjectSet
    checkPropertyExist mbObj = case mbObj ^? _Just . prop of -- TODO: there is a conversion func Maybe -> Either.
            Just x -> E.right x 
            Nothing -> E.left $ eNoSuchProperty (nameProperty prop)

save :: Object -> Eval ()
save obj = do
    let d = obj ^. singular objectDislocation
    ctxTransactionMap . at d .= Just obj

-- evaluation

setupActedObject :: Object -> EvalState ()
setupActedObject obj = ctxActedObject .= Just obj
dropActedObject :: EvalState ()
dropActedObject = ctxActedObject .= Nothing

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

withProperty prop act = doWithProperty :: Eval ()
  where
    doWithProperty = do
        objs <- having prop
        evalTransact act objs

evalTransact :: Eval String -> Objects -> Eval ()
evalTransact act [] = return ()
evalTransact act (o:os) = do
    ctx <- get
    let (_, newCtx) = runState (transact act o) ctx
    put newCtx
    evalTransact act os

evaluate scenario = evalState (runEitherT scenario)
execute scenario = execState (runEitherT scenario)

