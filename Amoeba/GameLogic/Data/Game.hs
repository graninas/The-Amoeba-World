module GameLogic.Data.Game where

import System.Random

import GameLogic.Data.World
import GameLogic.Data.Strategy

data Game = Game { gWorld :: World
                 , gStrategies :: Strategies
                 }
  deriving (Show, Read, Eq)

initialGame  = Game emptyWorld emptyStrategies
mkGame world = Game world emptyStrategies

updateGame (Game w strategies) = undefined
