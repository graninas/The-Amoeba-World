module World.Player where

import World.Types

data Player = Player PlayerId
  deriving (Show, Read, Eq)

type Players = [Player]

-- Ordinary players have Id > 0
player1 = Player 1
player2 = Player 2

-- Special 'players'
dummyPlayer = Player 0
stonePlayer = Player (-1)
canyonPlayer = Player (-2)
resourcesPlayer = Player (-3)

conflictPlayer = Player (-4)

obstaclePlayers = [stonePlayer, canyonPlayer, resourcesPlayer]


isObstaclePlayer pl = pl `elem` obstaclePlayers
isOrdinaryPlayer (Player pId) = pId > 0