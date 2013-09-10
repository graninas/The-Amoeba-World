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
import World.GameMapUpdater
import World.Id

import System.Random
import qualified Data.List as L
import qualified Data.Either as E
import qualified Control.Monad.Reader as R
import Control.Monad (liftM)

plasmaEmitent = let
    sqList = 0 : [x * x | x <- [1,3..]]
    ringsList = zipWith (-) (tail sqList) sqList
    in drop 1 ringsList

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
                     , karyonAge :: Int
                     , karyonBound :: Point -> Bound }

instance Id Karyon where
    getId = karyonId

instance Active Karyon where
    activate = activateKaryon
    ownedBy = karyonPlayer
    name (Karyon{}) = "Karyon"
    
instance Descripted Karyon where
    description  = show . mkSerializable

karyon :: ItemId -> Player -> Energy -> Point -> (Point, Karyon)
karyon kId pl e pos = (pos, Karyon kId pl e ordinalKaryonBound)

data ActivationContext = ActivationContext { activationItem :: Karyon
                                           , activationPiecePoint :: Point
                                           , activationPieceShift :: Shift }

type ActivationData = (World, Annotations, Energy)

activateKaryon :: Point -> Karyon -> World -> (World, Annotations)
activateKaryon p k@(Karyon kId pl e age _) w = let
    (w', anns, e') = emitKaryonPlasma p k w
    res = updateKaryon p k { karyonEnergy = e', karyonAge = age + 1 } (w', anns)
    in res

emitKaryonPlasma :: Point -> Karyon -> World -> (World, Annotations, Energy)
emitKaryonPlasma p k@(Karyon kId pl e age _) w = let
    ageEmitent = head . drop age $ plasmaEmitent
    emitent = min e ageEmitent
    (w', anns) = emitPlasma emitent p pl w
    in (w', anns, e - emitent)

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
    in (w { worldMap = alterGameMap [updIts] (worldMap w) }, anns ++ [ann])
    
ordinalKaryonBound :: Point -> Bound
ordinalKaryonBound p = circleBound p ordinalGrow

karyonEnergyUpdatedAnnotation p pl e = annotation $ showPointAndPlayer p pl ++ " Karyon energy updated: " ++ show e


updateKaryonEnergyAnnotation :: Point -> Karyon -> Annotation
updateKaryonEnergyAnnotation p k@(Karyon _ pl e _ _) = karyonEnergyUpdatedAnnotation p pl e
updateKaryonEnergyAnnotation _ _ = error "Not implemented"
