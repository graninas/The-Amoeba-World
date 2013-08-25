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
  activate w p _ = inactive w p
  ownedBy p@(Plasma{}) = plasmaPlayer p
  ownedBy p@(ConflictedPlasma{}) = conflictedOwner p

instance Descripted Plasma where
    description = show

plasma :: Int -> Player -> Plasma
plasma = Plasma

conflictedPlasma :: ItemId -> Player -> Players -> Plasma
conflictedPlasma = ConflictedPlasma

conflictatePlasma :: Plasma -> Players -> Plasma
conflictatePlasma (ConflictedPlasma plId owner ps1) ps2 = 
    ConflictedPlasma plId owner (ps1 `L.union` ps2)
conflictatePlasma (Plasma plId pl) ps =
    ConflictedPlasma plId conflictPlayer ([pl] `L.union` ps)
  

plasmaConstructor :: Player -> ItemId -> ActiveItem
plasmaConstructor pl pId = packItem $ plasma pId pl

conflictedPlasmaConstructor :: Players -> ItemId -> ActiveItem
conflictedPlasmaConstructor pls pId = packItem $ conflictedPlasma pId conflictPlayer pls

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
        items | isOnePlayerHere pl items || isNonePlayersHere pl items -> Left (GrowOver toPoint)
              | isObstacleItem items -> Left StopGrow
              | otherwise -> let -- Enemies are here!
                                 pls = L.union (getPlayers items) [pl]
                                 conflAct = addSingleConflictedAction toPoint (conflictedPlasmaConstructor pls) pls
                                 act = addSingleActiveAction toPoint (plasmaConstructor pl)
                             in return (conflAct : act : acts)

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
    