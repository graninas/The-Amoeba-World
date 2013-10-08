{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import Control.Monad
import Control.Lens
import Control.Applicative
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Maybe (fromJust, isJust, listToMaybe)
import Prelude hiding (read)

import GameLogic.Geometry
import GameLogic.Object
import qualified GameLogic.GenericWorld as GW
import Misc.Descriptions

data EvalError = NoSuchProperty String
  deriving (Show, Read, Eq)

type EvalType ctx res = EitherT EvalError (State ctx) res
type Eval res = EvalType EvaluationContext res

data EvaluationContext = EvaluationContext { _ctxNextRndNum :: Eval Int
                                           , _ctxObjectAt :: Point -> Eval (Maybe Object)
                                           , _ctxObjects :: Eval Objects
                                           }

makeLenses ''EvaluationContext

-- context

context = EvaluationContext

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
query q = liftM (filter q) objects

find :: (Object -> Bool) -> Eval (Maybe Object)
find q  = liftM listToMaybe (query q) :: Eval (Maybe Object)

single :: (Object -> Bool) -> Eval Object
single q = liftM fromJust (find q)

read obj prop = case obj ^? prop of
    Just x -> E.right x 
    Nothing -> E.left $ NoSuchProperty (nameProperty prop)

{-
whenIt prop pred = do
    mbObj <- use ctxActedObject
    return $ mbObj >>= maybeStored prop pred
-}

-- evaluation

withProperty prop act = doWithProperty :: Eval ()
  where
    doWithProperty = do
        objs <- having prop
        mapM_ act objs

evaluate scn = evalState (runEitherT scn)

