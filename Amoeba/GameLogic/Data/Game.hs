module GameLogic.Data.Game where

import System.Random

import GameLogic.Data.World

data Game = Game { gWorld :: World
                 , gRndGen :: StdGen }
  deriving (Show)

instance Eq Game where
    (Game w1 g1) == (Game w2 g2) = (w1 == w2) && (show g1 == show g2)

initialGame seed = Game emptyWorld (mkStdGen seed)
mkGame world seed = Game world (mkStdGen seed)
