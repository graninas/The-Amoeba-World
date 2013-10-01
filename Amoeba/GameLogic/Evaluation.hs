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

(~&~) p1 p2 = \obj -> f (p1 obj) (p2 obj)
  where
    f Nothing _ = Nothing
    f _ Nothing = Nothing
    f (Just x) (Just y) = Just (x && y)

infixr 3 ~&~

isJustTrue (Just x) = x
isJustTrue Nothing = False

maybeStored prop pred obj = let
    mbVal = obj ^? prop
    mbRes = liftM pred mbVal
    in if isJustTrue mbRes
            then mbVal
            else Nothing

is prop val = isJust . maybeStored prop (val ==)
suchThat = isJust . maybeStored
justAll _ = True

query q = liftM (filter q) objects :: Eval Objects
find q  = liftM listToMaybe (query q) :: Eval (Maybe Object)

read prop = use $ ctxActedObject . to fromJust . singular prop
readIf prop q = do
    mbObj <- ctxActedObject
    let checked = mbObj >>= check prop q
    if isJustTrue checked
        then mbObj >>= (^? prop)
        else Nothing

{-
data Query p b = Match { _queryProperty :: p
                       , _queryPredicate :: b }
               | JustAll
-}
--check prop pred obj = Match (obj ^? prop) pred
--query MatchAll = objects :: Eval Objects
--query (Match prop pred) = liftM (filter (isJustTrue . p)) objects :: Eval Objects
--justAll = JustAll

-- resolving

-- transact :: Object -> (Collision -> Bool) -> a -> a -> b
-- transact = undefined