{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad
import Control.Lens
import Control.Applicative
import Data.Monoid
import Data.Maybe (fromJust, isJust, listToMaybe)
import Prelude hiding (read)

import GameLogic.Geometry
import GameLogic.Object

type EvalType a b = State a b
type Eval a = EvalType EvaluationContext a
type ObjectedEval a = Eval a

data EvaluationContext = EvaluationContext { _ctxNextRndNum :: Eval Int
                                           , _ctxObjectAt :: Point -> Eval (Maybe Object)
                                           , _ctxObjects :: Eval Objects
                                           , _ctxActedObject :: Maybe Object }

makeLenses ''EvaluationContext

-- context

context rndF objectsAtF objectsF = EvaluationContext rndF objectsAtF objectsF Nothing

nextRndNum :: Eval Int
nextRndNum = get >>= _ctxNextRndNum

objectAt :: Point -> Eval (Maybe Object)
objectAt p = get >>= flip _ctxObjectAt p

objects :: Eval Objects
objects = get >>= _ctxObjects

having prop = liftM (filter (has prop)) objects

forObject obj act = do
    ctxActedObject .= Just obj
    act
    ctxActedObject .= Nothing

with prop act = do
    objs <- having prop
    mapM_ (`forObject` act) objs

-- querying

(~&~) p1 p2 obj = p1 obj && p2 obj

infixr 3 ~&~

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
query q = liftM (filter q) objects

find :: (Object -> Bool) -> Eval (Maybe Object)
find q  = liftM listToMaybe (query q) :: Eval (Maybe Object)

-- TODO: make it safe.
read prop = use $ ctxActedObject . to fromJust . singular prop

whenIt prop pred = do
    mbObj <- use ctxActedObject
    return $ mbObj >>= maybeStored prop pred

-- resolving

-- transact :: Object -> (Collision -> Bool) -> a -> a -> b
-- transact = undefined