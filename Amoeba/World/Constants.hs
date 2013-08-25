module World.Constants where

import World.Geometry
import World.Stochastic
import World.Types

growProbabilities :: GrowProbabilities
growProbabilities = [ (left, DirectionProbability 50 25 0 25)
                    , (up, DirectionProbability 25 50 25 0)
                    , (right, DirectionProbability 0 25 50 25)
                    , (down, DirectionProbability 25 0 25 50) ]
                    
ordinalGrow :: Radius
ordinalGrow = 10.0

invalidId :: ItemId
invalidId = -1