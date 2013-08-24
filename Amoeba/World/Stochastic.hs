module World.Stochastic where

import World.Geometry
import World.World

import Data.Word

type RandomNumber = Int

type DirectionProbability = DirectionProbability
                            { probLeft  :: Int
                            , probUp    :: Int
                            , probRight :: Int
                            , probDown  :: Int }
  deriving (Show, Read, Eq)
    
probabilityRange = (0, 99)


zeroProbability = DirectionProbability 0 0 0 0

getDirectionProbability baseDir probs = case lookup dir probs of
    Nothing -> zeroProbability
    Just p = p

getRandomDirection :: Int -> DirectionProbability -> Direction
getRandomDirection rndNum (DirectionProbability l u r d)
    | rndNum >  0         && rndNum < l             && l /= 0 = left
    | rndNum >= l         && rndNum < l + u         && u /= 0 = up
    | rndNum >= l + u     && rndNum < l + u + r     && r /= 0 = right
    | rndNum >= l + u + r && rndNum < l + u + r + d && d /= 0 = down
    | otherwise = zeroProbability

getSafeRandomDirection :: Int -> DirectionProbability -> Either String Direction
getSafeRandomDirection rndNum prob@(DirectionProbability l u r d) = 
    case getRandomDirection rndNum prob of
        zeroProbability -> Left ("Not applicable rndNum: " ++ show rndNum ++ " to prob: " ++ show prob)
        p -> Right p

triedDirsChecker triedDirs | length triedDirs >= 3 = Left "3 directions tried"
                           | otherwise = Right ()