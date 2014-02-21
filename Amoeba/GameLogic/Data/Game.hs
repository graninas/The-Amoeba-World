{-# LANGUAGE TemplateHaskell #-}

module GameLogic.Data.Game where

import Control.Lens
import System.Random

import GameLogic.Data.World

data Game = Game { _world :: World
                 , _rndGen :: StdGen }
  deriving (Show)

instance Eq Game where
    (Game w1 g1) == (Game w2 g2) = (w1 == w2) && (show g1 == show g2)

makeLenses ''Game

initialGame seed = Game emptyWorld (mkStdGen seed)

isGameEmpty game = game ^. world == emptyWorld

objects :: Lens' Game WorldMap
objects = world.worldMap

putObject p constructor = objects . at p ?~ constructor p