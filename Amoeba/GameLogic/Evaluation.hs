{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad
import Control.Lens
import Data.Maybe (fromJust)
import Prelude hiding (read)

import GameLogic.Geometry
import GameLogic.Object

type Eval a = State EvaluationContext a

data EvaluationContext = EvaluationContext { _ctxNextRndNum :: Eval Int
                                           , _ctxObjectAt :: Point -> Eval Object
                                           , _ctxObjects :: Eval Objects
                                           , _ctxActedObject :: Maybe Object }

makeLenses ''EvaluationContext

nextRndNum :: Eval Int
nextRndNum = get >>= _ctxNextRndNum

objectAt :: Point -> Eval Object
objectAt p = get >>= flip _ctxObjectAt p

objectsWith prop = liftM (filter (has prop)) (get >>= _ctxObjects)

forObject obj act = do
    ctxActedObject .= Just obj
    act
    ctxActedObject .= Nothing

with prop act = do
    objs <- objectsWith prop
    mapM_ (`forObject` act) objs

read prop = use $ ctxActedObject . to fromJust . singular prop

trans :: Object -> (Collision -> Bool) -> a -> a -> b
trans = undefined