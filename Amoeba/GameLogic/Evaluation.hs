module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad
import Control.Lens

import GameLogic.Geometry
import GameLogic.Object

type Eval a = State EvaluationContext a

data EvaluationContext = EvaluationContext { ctxNextRndNum :: Eval Int
                                           , ctxObjectAt :: Point -> Eval Object
                                           , ctxObjects :: Eval Objects }
nextRndNum :: Eval Int
nextRndNum = get >>= ctxNextRndNum

objectAt :: Point -> Eval Object
objectAt p = get >>= flip ctxObjectAt p

objectsWith prop = liftM (filter (has prop)) (get >>= ctxObjects)

with prop act = do
    objs <- objectsWith prop
    mapM_ act objs

trans :: Object -> (Collision -> Bool) -> a -> a -> b
trans = undefined