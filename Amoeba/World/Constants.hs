module World.Constants where

import World.World
import World.Player
import World.Geometry
import World.Stochastic

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E


growProbabilities :: GrowProbabilities
growProbabilities = [ (left, DirectionProbability 50 25 0 25)
                    , (up, DirectionProbability 25 50 25 0)
                    , (right, DirectionProbability 0 25 50 25)
                    , (down, DirectionProbability 25 0 25 50) ]
                    
ordinalGrow :: Radius                    
ordinalGrow = 10.0