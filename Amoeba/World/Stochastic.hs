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

getDirectionProbability baseDir probs = Maybe.fromMaybe zeroProbability (lookup baseDir probs)

getRandomDirection :: RandomNumber -> DirectionProbability -> Direction
getRandomDirection rndNum (DirectionProbability l u r d)
    | rndNum >  0         && rndNum < l             && l /= 0 = left
    | rndNum >= l         && rndNum < l + u         && u /= 0 = up
    | rndNum >= l + u     && rndNum < l + u + r     && r /= 0 = right
    | rndNum >= l + u + r && rndNum < l + u + r + d && d /= 0 = down
    | otherwise = zeroPoint

getSafeRandomDirection :: RandomNumber -> DirectionProbability -> Either String Direction
getSafeRandomDirection rndNum prob@(DirectionProbability l u r d) = 
    let dir = getRandomDirection rndNum prob in
    if dir == zeroPoint
    then Left ("Not applicable rndNum: " ++ show rndNum ++ " to prob: " ++ show prob)
    else Right dir
    
triedDirsChecker :: [Direction] -> Either String ()  -- TODO: move it
triedDirsChecker triedDirs | length triedDirs >= 3 = Left "3 directions tried"
                           | otherwise = Right ()

chooseRandomDir :: GrowProbabilities -> StdGen -> Direction -> [Direction] -> Either String (StdGen, Direction)
chooseRandomDir probs g0 dir triedDirs = do
    triedDirsChecker triedDirs -- TODO: move it
    let (rndNum, g1) = randomProbabilityNum g0
    let dirProb = getDirectionProbability dir probs
    rndDir <- getSafeRandomDirection rndNum dirProb
    return (g1, rndDir)