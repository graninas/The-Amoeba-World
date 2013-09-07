module GameLogic.Scene where

import GameView.Render
import GameView.SceneGraph
import World.Player
import World.Geometry

import Data.Monoid

data Scale = Scale Float
  deriving (Show, Read, Eq)

data View = View { viewPlayer :: Player
                 , viewCenter :: Point
                 , viewScale :: Scale
                 }
  deriving (Show, Read, Eq)

scene, menu :: SceneGraph
scene = under rWorld $ rFrame `under` rStats
menu = under scene $ rStartNewGame <> rQuit

rWorld = undefined
rFrame = undefined
rStats = undefined
rStartNewGame = undefined
rQuit = undefined