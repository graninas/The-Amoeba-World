module GameLogic.AI where

import GameLogic.Geometry
import GameLogic.Object

data PathStrategy = WithLast
type TrackAction = Object -> Point -> Object

-- For some Object, track points with action on the object.
track :: Path -> PathStrategy -> TrackAction -> Object -> Object
track ps strategy act obj = foldl act obj (enrollPathStrategy strategy ps) 

withLast :: PathStrategy
withLast = WithLast

enrollPathStrategy :: PathStrategy -> Points -> Points
enrollPathStrategy WithLast [] = []
enrollPathStrategy WithLast ps = [last ps]




move p (StraightMoving s d) = moveStraight s p d
path p (StraightMoving s d) = scanl advance p (replicate s d)
