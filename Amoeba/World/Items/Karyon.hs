module World.Items.Karyon where

import Prelude hiding (Bounded)

import World.Types
import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Items.Plasma
import World.Descripted
import World.WorldMapUpdater
import World.Id

import System.Random
import qualified Data.List as L
import qualified Data.Either as E
import qualified Control.Monad.Reader as R
import Control.Monad (liftM)

karyonPieceActivateCount = 1

-- TODO: World bounds, conflicting cells, other deals
-- TODO: energy collecting.
-- TODO: annotations on impossible grow.
-- TODO: testing!
-- TODO: fix bug with multiple players in annotation:
-- [V3 6 6 0, Player 1] Already conflicted here with players [Player 1,Player 1,Player 1,Player 2]

-- TODO: next plasma growing - with bounds, but with A* algorithm wich should
-- search the nearest possible cell for growing.

data Karyon = Karyon { karyonId :: ItemId
                     , karyonPlayer :: Player
                     , karyonEnergy :: Energy
                     , karyonFillers :: [Karyon]
                     , karyonBound :: Point -> Bound }
            | KaryonFiller { karyonId :: ItemId
                           , karyonPlayer :: Player
                           , karyonFillerShift :: Shift }

instance Id Karyon where
    getId = karyonId

instance Active Karyon where
    activate = activateKaryon
    ownedBy = karyonPlayer
    name _ = "Karyon"
    
instance Descripted Karyon where
    description  = show . mkSerializable

karyon :: ItemId -> Player -> Energy -> Point -> [(Point, Karyon)]
karyon kId pl e pos = kayronCell : pointedFillers
  where
    kayronCell = (pos, Karyon kId pl e fillers ordinalKaryonBound)
    pointedFillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller kId pl sh)
    fillers = map snd pointedFillers

data ActivationContext = ActivationContext { activationItem :: Karyon
                                           , activationPiecePoint :: Point
                                           , activationPieceShift :: Shift }

type ActivationData = (World, Annotations, Energy)

activateKaryon :: Point -> Karyon -> World -> (World, Annotations)
activateKaryon p k@(KaryonFiller{}) w = inactive p k w
activateKaryon p k@(Karyon kId pl e fillers _) w = let
    shifts = map karyonFillerShift fillers
    f actData = foldr (runPieceActivation p k) actData shifts
    iteraties = iterate f (w, [activationAnnotation p k], e)
    (w', anns, e') = head . drop karyonPieceActivateCount $ iteraties
    in if e' /= e then updateKaryon p k { karyonEnergy = e' } (w', anns)
                  else (w', anns)

runPieceActivation :: Point -> Karyon -> Shift -> ActivationData -> ActivationData
runPieceActivation p k sh actData = let
    actContext = ActivationContext k p sh
    in R.runReader (activatePiece actData) actContext

askIsCornerPiece :: R.Reader ActivationContext Bool
askIsCornerPiece = do
    sh <- liftM activationPieceShift R.ask
    return $ isCornerShift sh

askSubDirections :: R.Reader ActivationContext (Direction, Direction)
askSubDirections = do
    sh <- liftM activationPieceShift R.ask
    return (subDirection1 sh, subDirection2 sh)

askDirection :: R.Reader ActivationContext Direction
askDirection = liftM (direction . activationPieceShift) R.ask 

activatePiece :: ActivationData -> R.Reader ActivationContext ActivationData
activatePiece actData = do
    isCornerPiece <- askIsCornerPiece
    if isCornerPiece then activateCornerPiece actData
                     else activateOrdinaryPiece actData

activateCornerPiece :: ActivationData -> R.Reader ActivationContext ActivationData
activateCornerPiece actData = do
    (subDir1, subDir2) <- askSubDirections
    activatePieceGrowing subDir1 actData >>= activatePieceGrowing subDir2

activateOrdinaryPiece :: ActivationData -> R.Reader ActivationContext ActivationData
activateOrdinaryPiece actData = do
    dir <- askDirection
    activatePieceGrowing dir actData

activatePieceGrowing :: Direction -> ActivationData -> R.Reader ActivationContext ActivationData
activatePieceGrowing _   actData@(w, anns, 0) = return actData
activatePieceGrowing dir actData@(w, anns, e) = do
    (pl, bounds, p) <- askLocals
    case growPlasma pl bounds p dir w of
        Left ann -> return (w, anns ++ [ann], e)
        Right (w', anns') -> return (w', anns ++ anns', e-1)
  where
    askLocals = do
        (ActivationContext k p _) <- R.ask
        return (karyonPlayer k, [karyonBound k p], p)

data SerializibleKaryon = SKaryon { sKaryonId :: ItemId
                                  , sKaryonPlayer :: Player
                                  , sKaryonEnergy :: Energy
                                  , sKaryonFillers :: [SerializibleKaryon]
                                  , sKaryonBound :: Bound }
                        | SKaryonFiller { sKaryonId :: ItemId
                                        , sKaryonPlayer :: Player
                                        , sKaryonFillerDir :: Direction }
  deriving (Show, Read)
    
mkSerializable (Karyon kId pl e fs b) = SKaryon kId pl e (map mkSerializable fs) (b zeroPoint)
mkSerializable (KaryonFiller kId pl sh) = SKaryonFiller kId pl (direction sh)

updateKaryon :: Point -> Karyon -> (World, Annotations) -> (World, Annotations)
updateKaryon p k (w, anns) = let
    updIts = replaceItemFunc (p, k)
    ann = updateKaryonEnergyAnnotation p k
    in (w { worldMap = alterWorldMap [updIts] (worldMap w) }, anns ++ [ann])
    
ordinalKaryonBound :: Point -> Bound
ordinalKaryonBound p = circleBound p ordinalGrow

karyonEnergyUpdatedAnnotation p pl e = annotation $ showPointAndPlayer p pl ++ " Karyon energy updated: " ++ show e


updateKaryonEnergyAnnotation :: Point -> Karyon -> Annotation
updateKaryonEnergyAnnotation p k@(Karyon _ pl e _ _) = karyonEnergyUpdatedAnnotation p pl e
updateKaryonEnergyAnnotation _ _ = error "Not implemented"
