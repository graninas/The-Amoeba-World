module World.Karyon where

import Prelude hiding (Bounded)

import World.World
import World.Player
import World.Geometry
import World.Stochastic

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E


type Energy = Int

data Karyon = Karyon { karyonPlayer :: Player
                     , karyonEnergy :: Energy
                     , karyonFillers :: [Karyon] }
            | KaryonFiller { fillerRelativeShift :: Shift }


instance Active Karyon where
  activate = activateKaryon
  
instance Bounded Karyon where
  bounds _ pos = BoundCircle pos 10
  
karyon :: Player -> Int -> Point -> [(Point, Items)]
karyon pl e pos = map mkItems (kayronCell : pointedFillers)
  where
    kayronCell = (pos, Karyon pl e fillers)
    pointedFillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller sh)
    fillers = map snd pointedFillers
    mkItems (p, k) = (p, makeItems [k])
    

    
activateKaryon :: Karyon -> Point -> WorldMutator -> World -> WorldMutator
activateKaryon k@(KaryonFiller _) p m w = inactive p m w
activateKaryon k@(Karyon pl e fillers) p mutator w = let

  dummy = activateKayronPiece pl mutator w e p (fillerRelativeShift . head $ fillers)
  in undefined
  
activateKayronPiece :: Player -> WorldMutator -> World -> Energy -> Point -> Shift -> Either String (Energy, WorldMutator)
activateKayronPiece pl wm w e kayronPoint pieceShift = if isCornerShift pieceShift
    then do
       let cornerSide1Func = activateKayronPiece' pl wm e kayronPoint (subShift1 pieceShift)
       let cornerSide2Func (e', wm') = activateKayronPiece' pl wm' e' kayronPoint (subShift2 pieceShift)
       E.either Left cornerSide2Func cornerSide1Func
    else activateKayronPiece' pl wm e kayronPoint pieceShift

  where
    activateKayronPiece' :: Player -> WorldMutator -> Energy -> Point -> Shift -> Either String (Energy, WorldMutator)
    activateKayronPiece' pl wm' e' kayronPoint subShift | e' <= 0   = Left "No energy" 
                                                        | otherwise = do
        growedWm <- growPlasma pl wm' w (subShift kayronPoint) (direction subShift)
        return (e' - 1, growedWm)
        



growProbabilities :: [(Direction, DirectionProbability)]
growProbabilities = [ (left,  DirectionProbability 50 25 0 25)
                    , (up,    DirectionProbability 25 50 25 0)
                    , (right, DirectionProbability 0 25 50 25)
                    , (down,  DirectionProbability 25 0 25 50) ]


chooseRandomDir :: StdGen -> Direction -> [Direction] -> Either String (StdGen, Direction)
chooseRandomDir g0 dir triedDirs = do
    triedDirsChecker triedDirs
    let (rndNum, g1) = randomProbabilityNum g0
    let dirProb = getDirectionProbability dir growProbabilities
    rndDir <- getSafeRandomDirection rndNum dirProb -- TODO
    return (g1, rndDir)


data Plasma = Plasma { plasmaPlayer :: Player }
instance Active Plasma where
  activate _ = inactive

plasma :: Player -> Plasma
plasma = Plasma

grow' :: Player -> Actions -> World -> Point -> Direction -> Either String Actions
grow' pl acts w toPoint dir = do
    emptyCellChecker toPoint w
    let act = addSingleActive toPoint (plasma pl)
    return (act : acts)

grow :: Player
     -> Actions
     -> StdGen
     -> World
     -> Point
     -> Direction
     -> [Direction]
     -> Either String WorldMutator    
grow pl acts g0 w fromPoint dir triedDirs = do
    (g1, rndDir) <- chooseRandomDir g0 dir triedDirs
    let growFunc = grow' pl acts w (movePoint fromPoint rndDir) dir
    let nextDir = nextDirection dir
    let tryNext _ = grow pl acts g1 w fromPoint nextDir (dir : triedDirs)
    let newWm acts = Right $ createWorldMutator g1 acts
    E.either tryNext newWm growFunc

growPlasma :: Player -> WorldMutator -> World -> Point -> Direction -> Either String WorldMutator
growPlasma pl wm@(WorldMutator acts g0) w piecePoint dir = grow pl acts g0 w piecePoint dir []
    
    
    
    
    
        
        