module World.Player where


data Player = Player Int
  deriving (Show, Read, Eq)




player1 = Player 1
player2 = Player 2

stonePlayer = Player (-1)
canyonPlayer = Player (-2)
resourcesPlayer = Player (-3)

obstaclePlayers = [stonePlayer, canyonPlayer, resourcesPlayer]