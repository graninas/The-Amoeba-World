module Application.GameFlow where

import Control.Wire

data GameFlow = GameFlow { gameFlowMove :: Int
                         , gameFlowEvents :: [(Time, Int, String)]
                         }
    deriving (Show)
