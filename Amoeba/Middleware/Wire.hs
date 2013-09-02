module Middleware.Wire where

import Control.Wire
import Prelude hiding ((.), id)
import Control.Monad.State

import World.World

data GameFlow = GameFlow { gameFlowMove :: Int
                         , gameFlowEvents :: [(Time, Int, String)]
                         }
    deriving (Show)

type WStateIO = StateT World IO
type WWire a b = Wire () WStateIO a b