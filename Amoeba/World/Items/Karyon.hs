module World.Items.Karyon where

import Prelude hiding (Bounded)

import World.Types
import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Items.Plasma

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E

-- TODO: World bounds, conflicting cells, other deals
-- TODO: ++ Modify energy actions!

data Karyon = Karyon { karyonId :: ItemId
                     , karyonPlayer :: Player
                     , karyonEnergy :: Energy
                     , karyonFillers :: [Karyon]
                     , karyonBound :: Bound }
            | KaryonFiller { karyonId :: ItemId
                           , karyonPlayer :: Player
                           , fillerRelativeShift :: Shift }

instance Id Karyon where
    getId = karyonId

decreaseEnergyModificatorId = 1 -- TODO: implement better mechanism
increaseEnergyModificatorId = 2

instance Active Karyon where
  activate = activateKaryon
  ownedBy = karyonPlayer
  
ordinalKaryonBound :: Point -> Bound
ordinalKaryonBound p = Circle p ordinalGrow

karyon :: ItemId -> Player -> Energy -> Point -> [(Point, Items)]
karyon kId pl e pos = map mkItems (kayronCell : pointedFillers)
  where
    kayronCell = ( pos, Karyon kId pl e fillers (ordinalKaryonBound pos) )
    pointedFillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller kId pl sh)
    fillers = map snd pointedFillers
    mkItems (p, k) = (p, makeItems [k])

activateKaryon :: World -> Point -> Karyon -> WorldMutator -> WorldMutator
activateKaryon w p k@(KaryonFiller{}) mutator = inactive w p mutator
activateKaryon w p k@(Karyon kId _ e fillers _) mutator = let
    e' = calculateEnergyConsumption kId e mutator
    shifts = map fillerRelativeShift fillers
    in snd $ foldr (activatePieces w p k) (e', mutator) shifts

energyAction :: (Energy -> Energy) -> Int -> Point -> Karyon -> Action
energyAction f mId p k = modifyItemAction p k mId mod
  where
    mod :: Karyon -> Karyon
    mod k@(KaryonFiller {}) = error "Error. This should'n be."
    mod k@(Karyon _ _ e _ m) = k { karyonEnergy = f e }

activatePieces :: World -> Point -> Karyon -> Shift -> (Energy, WorldMutator) -> (Energy, WorldMutator)
activatePieces w p k@(Karyon _ pl _ _ bound) sh (e, wm) = case activateKaryonPiece bound pl w e p sh wm of
    Left (reservedEnergy, newEnergy, WorldMutator acts g) -> let
        reserveActions = replicate reservedEnergy (energyAction (+1) increaseEnergyModificatorId p k)
        consumeActions = replicate (e - newEnergy) (energyAction ((-)1) decreaseEnergyModificatorId p k)
        newActions = reserveActions ++ consumeActions
        in (newEnergy, WorldMutator (acts ++ newActions) g)
    Right (newEnergy, WorldMutator acts g) -> let
        consumeActions = replicate (e - newEnergy) (energyAction ((-)1) decreaseEnergyModificatorId p k)
        in (newEnergy, WorldMutator (acts ++ consumeActions) g)
activatePieces _ _ _ _ eWm = error "Error. This shouldn't be."

calculateEnergyConsumption :: Int -> Energy -> WorldMutator -> Energy
calculateEnergyConsumption kId e mutator = let
    decreased = getModificatorActions kId decreaseEnergyModificatorId mutator
    increased = getModificatorActions kId increaseEnergyModificatorId mutator
    in validateEnergy $ e - length increased + length decreased

activateKaryonPiece :: Bound -> Player -> World -> Energy -> Point -> Shift -> WorldMutator -> Either (Energy, Energy, WorldMutator) (Energy, WorldMutator)
activateKaryonPiece karyonBound pl w e kayronPoint pieceShift wm = if isCornerShift pieceShift
    then E.either (reservableGrow 2) (reservableGrow 1) grow1
    else E.either (reserveEnergy 1) return grow0
  where
      reserveEnergy cnt (e, wm) = Left (cnt, e, wm)
      growFunc subShiftFunc (e', wm') = activateKayronPiece' karyonBound pl wm' w e' kayronPoint (subShiftFunc pieceShift)
      grow0 = growFunc id (e, wm)
      grow1 = growFunc subShift1 (e, wm)
      grow2 = growFunc subShift2
      reservableGrow reserve d = E.either (reserveEnergy reserve) return (grow2 d)

decreaseEnergy e | e <= 0 = 0
                 | otherwise = e - 1
validateEnergy e | e <= 0 = 0
                 | otherwise = e

activateKayronPiece' :: Bound -> Player -> WorldMutator -> World -> Energy -> Point -> Shift -> Either (Energy, WorldMutator) (Energy, WorldMutator)
activateKayronPiece' karyonBound pl wm w e kayronPoint subShift
        | e <= 0 = Left (e, wm) -- No energy
        | otherwise = do
            let growPlasmaFunc = growPlasma karyonBound pl wm w (subShift kayronPoint) (direction subShift)
            let decreaseEnergyFunc wm' = Right (decreaseEnergy e, wm')
            let failActivation _ = Left (e, wm) -- Failed to grow 
            E.either failActivation decreaseEnergyFunc growPlasmaFunc