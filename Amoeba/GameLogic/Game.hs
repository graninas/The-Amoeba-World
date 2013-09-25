{-# LANGUAGE TemplateHaskell #-}

module GameLogic.Game where

import Control.Lens
import System.Random

import GameLogic.World


data Game = Game { _world :: World
                 , _rndGen :: StdGen }
  deriving (Show)

instance Eq Game where
    (Game w1 g1) == (Game w2 g2) = (w1 == w2) && (show g1 == show g2)


initialGame seed = Game emptyWorld (mkStdGen seed)


makeLenses ''Game

objects :: Lens' Game WorldMap
objects = world.worldMap

