module World.Items.Plasma where

import Prelude hiding (Bounded)

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E

data Plasma = Plasma { plasmaId :: ItemId
                     , plasmaPlayer :: Player }

instance Id Plasma where
  getId = plasmaId

instance Active Plasma where
  activate w p _ = inactive w p
  ownedBy = plasmaPlayer

plasma :: Int -> Player -> Plasma
plasma = Plasma

plasmaConstructor :: Player -> ItemId -> Plasma
plasmaConstructor pl pId = plasma pId pl

data GrowMode = GrowOver Point
              | StopGrow

grow' :: Bound
     -> Player
     -> Actions
     -> World
     -> Point
     -> Direction
     -> Either GrowMode Actions
grow' bound pl acts w toPoint dir
    | not $ inBounds toPoint bound = Left StopGrow
    | otherwise = case takeWorldItems toPoint w of
        NoItems -> let act = addSingleActive toPoint (plasmaConstructor pl)
                   in return (act : acts)
        items | isOnePlayerHere pl items -> Left (GrowOver toPoint)
              | isObstacle items -> Left StopGrow
              | otherwise -> let act = addSingleConflicted toPoint (plasmaConstructor pl) pl
                             in return (act : acts)

grow :: Bound
     -> Player
     -> Actions
     -> StdGen
     -> World
     -> Point
     -> Direction
     -> [Direction]
     -> Either GrowMode WorldMutator
grow bound pl acts g0 w fromPoint dir triedDirs = 
    case chooseRandomDir growProbabilities g0 dir triedDirs of
        Left _ -> Left StopGrow
        Right (g1, rndDir) -> do
            let growFunc = grow' bound pl acts w (movePoint fromPoint rndDir) dir
            let nextDir = nextDirection dir
            let tryNextDirectionFunc = grow bound pl acts g1 w fromPoint nextDir (dir : triedDirs)
            let successedGrowFunc acts = Right $ createWorldMutator g1 acts
            let tryGrowMode StopGrow = tryNextDirectionFunc
            let tryGrowMode (GrowOver p) = grow bound pl acts g1 w p dir []
            E.either tryGrowMode successedGrowFunc growFunc

growPlasma :: Bound -> Player -> WorldMutator -> World -> Point -> Direction -> Either String WorldMutator
growPlasma bound pl wm@(WorldMutator acts g) w piecePoint dir = do
    let growFunc = grow bound pl acts g w piecePoint dir []
    let failGrow _ = Left "No grow possible" 
    E.either failGrow return growFunc
    