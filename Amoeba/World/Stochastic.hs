module World.Stochastic where

import World.Geometry
import World.Types

import System.Random
import qualified Data.Maybe as Maybe

data DirectionProbability = DirectionProbability
                            { probLeft  :: RandomNumber
                            , probUp    :: RandomNumber
                            , probRight :: RandomNumber
                            , probDown  :: RandomNumber }
  deriving (Show, Read, Eq)
    
type GrowProbabilities = [(Direction, DirectionProbability)]

probabilityRange :: ProbabilityRange
probabilityRange = (0, 99)

randomProbabilityNum :: StdGen -> (RandomNumber, StdGen) 
randomProbabilityNum = randomR probabilityRange

zeroProbability = DirectionProbability 0 0 0 0

randomDirection :: RandomNumber -> DirectionProbability -> Maybe Direction
randomDirection rndNum (DirectionProbability l u r d)
    | rndNum >  0         && rndNum < l             && l /= 0 = Just left
    | rndNum >= l         && rndNum < l + u         && u /= 0 = Just up
    | rndNum >= l + u     && rndNum < l + u + r     && r /= 0 = Just right
    | rndNum >= l + u + r && rndNum < l + u + r + d && d /= 0 = Just down
    | otherwise = Nothing

chooseRandomDir :: StdGen -> Directions -> GrowProbabilities -> Maybe (StdGen, Direction, Directions)
chooseRandomDir _ [] _ = Nothing
chooseRandomDir g0 (dir:restDirs) probs =
    let (rndNum, g1) = randomProbabilityNum g0
        takeDirFunc = do rndDir <- lookup dir probs >>= randomDirection rndNum
                         return (g1, rndDir, restDirs)
    in case takeDirFunc of
        Nothing -> chooseRandomDir g1 restDirs probs
        result -> result
        
    
    
    