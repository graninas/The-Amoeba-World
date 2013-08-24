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


type Energy = Word

data Karyon = Karyon { karyonPlayer :: Player
                     , karyonEnergy :: Energy
                     , karyonFillers :: [Karyon] }
            | KaryonFiller { fillerRelativeShift :: Shift }


instance Active Karyon where
  activate = activateKaryon
  
instance Bounded Karyon where
  bounds _ = \pos -> BoundCircle pos 10
  
karyon :: Player -> Int -> Point -> [(Point, Karyon)]
karyon pl e pos = kayronCell : pointedFillers
  where
    kayronCell = (pos, Karyon pl e fillers)
    pointedFillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller sh)
    fillers = map snd pointedFillers
    

    
activateKaryon :: i -> Point -> WorldMutator -> World -> WorldMutator
activateKaryon k@(KaryonFiller _) = inactive k
activateKaryon k@(Karyon pl e fillers) p mutator w = let

  dummy = activateKayronPiece pl p mutator w e (fillerRelativeShift . head $ fillers)
  in undefined
  
activateKayronPiece :: Player -> Point -> WorldMutator -> World -> Energy -> Shift -> Either String (Energy, Mutator) -- TODO
activateKayronPiece pl p wm w e sh = case isCornerShift sh of
    True -> 
    False ->
  where
    activateKayronPiece' pl wm' e' subShift | e' == 0 = Left "No energy" 
    activateKayronPiece' pl wm' e' subShift | e' > 0 = do
        growedWm <- growPlasma pl p wm' w subShift
        return (e' - 1, growedWm)
        



growProbabilities :: [(Direction, DirectionProbability)]
growProbabilities = [ (left,  DirectionProbability 50 25 0 25)
                    , (up,    DirectionProbability 25 50 25 0)
                    , (right, DirectionProbability 0 25 50 25)
                    , (down,  DirectionProbability 25 0 25 50) ]



chooseRandomDir g0 dir triedDirs = do
    triedDirsChecker triedDirs
    let (rndNum, g1) = random probabilityRange g0
    let dirProb = getDirectionProbability dir growProbabilities
    (Right rndDir) <- getSafeRandomDirection rndNum dirProb -- is it impossible to chrash here?
    return (g1, rndDir)


data Plasma = Plasma { plasmaPlayer :: Player }
instance Active Plasma where
  activate = inactive

plasma :: Player -> Point -> [(Point, Plasma)]
plasma pl p = [(p, Plasma pl)]

grow' pl acts w toPoint dir = do
    emptyCellChecker p w
    let act = addActive toPoint (plasma pl)
    return (act : acts)

grow pl acts g0 w p dir triedDirs = do
    (g1, rndDir) <- cooseRandomDir g0 dir triedDirs
    let growFunc = grow' pl acts w (rndDir p) dir
    let nextDir = nextDirection dir
    let tryNext = grow pl acts g1 w p nextDir (dir : triedDirs)
    E.either tryNext createWorldMutator growFunc

growPlasma :: Player -> Point -> WorldMutator -> World -> Direction -> Either String WorldMutator
growPlasma pl p wm@(WorldMutator acts g0) w dir = do
    grow acts g0 pl wm w p dir []
    
    
    
    
    
        
        