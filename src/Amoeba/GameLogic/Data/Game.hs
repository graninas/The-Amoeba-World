module Amoeba.GameLogic.Data.Game (
    Game(..)
    , initialGame
    , mkGame
    , updateGame
    , getWorldMap
  ) where

import Amoeba.GameLogic.Data.World
import Amoeba.GameLogic.Data.Strategy

-- TODO: use lens

data Game = Game { gWorld :: World
                 , gStrategies :: Strategies
                 }
  deriving (Show, Read, Eq)

initialGame :: Game
initialGame  = Game emptyWorld emptyStrategies

mkGame :: World -> Game
mkGame world = Game world emptyStrategies

updateGame :: Game -> Game
updateGame _ = undefined

getWorldMap :: Game -> WorldMap
getWorldMap (Game (World wm _ _ _ _) _) = wm
