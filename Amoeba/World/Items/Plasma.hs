module World.Items.Plasma where

import Prelude hiding (Bounded)

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Types
import World.Id

import System.Random
import qualified Data.List as L
import qualified Data.Either as E

data Plasma = Plasma { plasmaId :: ItemId
                     , plasmaPlayer :: Player }
            | ConflictedPlasma { plasmaId :: ItemId
                               , conflictedOwner :: Player
                               , conflictedPlayers :: Players }
  deriving (Show, Read)

instance Id Plasma where
  getId = plasmaId

instance Active Plasma where
  activate = inactive
  ownedBy p@(Plasma{}) = plasmaPlayer p
  ownedBy p@(ConflictedPlasma{}) = conflictedOwner p

instance Descripted Plasma where
    description = show

plasma :: Point -> ItemId -> Player -> [(Point, Plasma)]
plasma p pId pl = [(p, Plasma pId pl)]

conflictedPlasma :: Point -> ItemId -> Player -> Players -> [(Point, Plasma)]
conflictedPlasma p pId pl pls = [(p, ConflictedPlasma pId pl pls)]

data GrowResult = CreepOver
                | GrowImpossible
                | Grow
                | TakeConflict Players
                | AlreadyConflicted Players
  deriving (Show, Read, Eq)

checkGrow :: Player -> Bounds -> Point -> World -> GrowResult
checkGrow pl bounds toPoint w
    | not $ inBounds toPoint bounds = GrowImpossible
    | otherwise = let items = takeWorldItems toPoint w
                  in case getPlayers items of
        []    -> Grow
        players | hasObstaclePlayer players -> GrowImpossible
                | isPlayerAlone pl players -> CreepOver
                | isPlayerHere pl players -> AlreadyConflicted (pl : players)
                | otherwise -> TakeConflict (pl : players)

defaultGrowDirs = [left, up, down, right]

conflictAnnotation p pls = annotation $ showPoint p ++ " Conflict of players: " ++ show pls
addingPlasmaAnnotation p pl = annotation $ showPointAndPlayer p pl ++ " Adding plasma"
addingConflictedPlasmaAnnotation p pl = annotation $ showPointAndPlayer p pl ++ " Adding conflicted plasma"

addPlasma :: Player -> Point -> World -> (World, Annotations)
addPlasma pl toPoint w@(World wm lId g) = let
    ann = addingPlasmaAnnotation toPoint pl
    newPlasma = plasma toPoint (lId + 1) pl
    newItems = addItemsFunc newPlasma
    in (World (updateWorldMap newItems wm) (lId + 1) g, [ann])

addConflictedPlasma :: Player -> Point -> Players -> World -> (World, Annotations)
addConflictedPlasma pl toPoint pls w@(World wm lId g) = let
    anns = [ conflictAnnotation toPoint pls
           , addingPlasmaAnnotation toPoint pl
           , addingConflictedPlasmaAnnotation toPoint pl ]
    newPlasma = plasma toPoint (lId + 1) pl
    newConflictedPlasma = conflictedPlasma toPoint (lId + 2) pl pls
    newItems = addItemsFunc (newPlasma ++ newConflictedPlasma)
    in (World (updateWorldMap newItems wm) (lId + 2) g, anns)

tryGrow :: Player -> Bounds -> (Point, Direction,  Directions)
     -> World -> Either String (World, Annotations)
tryGrow _ _ (_, _, []) _ = Left "No ways to grow"
tryGrow pl bounds (fromPoint, dir, availableDirs) w@(World wm lId g0) =
    case chooseRandomDir g0 availableDirs growProbabilities of
        Nothing -> Left "Random dir choosing failed"
        Just (g1, rndDir, restDirs) -> let toPoint = movePoint fromPoint rndDir
                                           w' = World wm lId g1
                                       in case checkGrow pl bounds toPoint w of
                Grow -> Right $ addPlasma pl toPoint w'
                CreepOver -> tryGrow pl bounds (toPoint, dir, defaultGrowDirs) w' -- Try next cell
                GrowImpossible -> tryGrow pl bounds (fromPoint, dir, restDirs) w' -- try another direction
                TakeConflict pls -> Right $ addConflictedPlasma pl toPoint pls w'
                AlreadyConflicted pls -> Left $ "Already conflicted in point " ++ show toPoint

growPlasma :: Player -> Bounds -> Point -> Direction -> World -> Either String (World, Annotations)
growPlasma pl bounds piecePoint dir = tryGrow pl bounds (piecePoint, dir, defaultGrowDirs)
    
