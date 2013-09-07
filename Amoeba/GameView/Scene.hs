module GameView.Scene where

import Render
import SceneGraph

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