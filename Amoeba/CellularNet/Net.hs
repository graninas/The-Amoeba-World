module CellularNet.Net where

import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random

type Energy = Int 

data Cell = Neuron Energy
          | Modulator Int
  deriving (Show, Read, Eq)

type Pos = (Int, Int)
type Net = M.Map Pos Cell

minModValue = -3
maxModValue = 3
minNeuronEnergy = 0
maxNeuronEnergy = 19

fieldMaxX = 10
fieldMaxY = 10

-- Modifications

neuronSaturation e = min e maxNeuronEnergy

-- Net structure

makeNullCell (x, y) (g, net) | even $ x + y = (g, M.insert (x, y) (Neuron    0) net)
                             | otherwise    = (g, M.insert (x, y) (Modulator 0) net)
    
makeRandomCell (x, y) (g, net) = let (val1, g')  = randomR (minNeuronEnergy, maxNeuronEnergy) g
                                     (val2, g'') = randomR (minModValue, maxModValue) g
                                 in case even $ x + y of
                                        True  -> (g',  M.insert (x, y) (Neuron    val1) net)
                                        False -> (g'', M.insert (x, y) (Modulator val2) net)

makeCell (actX1Net, actY1Net) (actX2Net, actY2Net) c@(x, y) d
    | x < actX1Net || x > actX2Net || y < actY1Net || y > actY2Net = makeNullCell c d
    | otherwise = makeRandomCell c d

makeRandomNet :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Int -> (StdGen, Net)
makeRandomNet (xSize, ySize) ((actX1, actY1), (actX2, actY2)) seed
    | actY2 < actY1 || actX2 < actX1 || xSize < actX2 || ySize < actY2 = error "Invalid arguments."
    | otherwise = let
        xNetSize = xSize * 2 + 1
        yNetSize = ySize * 2 + 1
        actX1Net = actX1 * 2 - 1
        actX2Net = actX2 * 2 + 1
        actY1Net = actY1 * 2 - 1
        actY2Net = actY2 * 2 + 1
        mapPoints = [(a, b) | a <- [1..xNetSize], b <- [1..yNetSize] ]
        g = mkStdGen seed
        in foldr (makeCell (actX1Net, actY1Net) (actX2Net, actY2Net)) (g, M.empty) mapPoints

-- Operations

type Signal = Int
instance Monoid Int where
  mappend = (+)
  mempty = 0

-- (LeftSignal, RightSignal) or (UpSignal, DownSignal)
type ModulatorIncome = (Signal, Signal)
type ModulatorIncomeMap = M.Map Pos ModulatorIncome

netRank = 4

updateIncome (p, i) = M.insertWith' (<>) p i

-- (LeftSignal, UpSignal, RightSignal, DownSignal)
type NeuronIncome = (Signal, Signal, Signal, Signal)
type NeuronIncomeMap = M.Map Pos NeuronIncome

downSignal  s = (0, 0, 0, s)
upSignal    s = (0, s, 0, 0)
leftSignal  s = (s, 0, 0, 0)
rightSignal s = (0, 0, s, 0)

targetForwardNeuronIncome  (x, y) s | odd x = ((x, y+1), upSignal   s)
                                    | odd y = ((x+1, y), leftSignal s)
                                    | otherwise = error $ "Invalid modulator index: " ++ show (x, y)
targetBackwardNeuronIncome (x, y) s | odd x = ((x, y-1), downSignal  s)
                                    | odd y = ((x-1, y), rightSignal s)
                                    | otherwise = error $ "Invalid modulator index: " ++ show (x, y)

income f p s v iMap net = let neuronIncome = f p (max (s + v) 0)
                          in (updateIncome neuronIncome iMap, net)

forward :: Pos -> Signal -> (NeuronIncomeMap, Net) -> (NeuronIncomeMap, Net)
forward p@(x, y) s (iMap, net) = case M.lookup p net of
    Nothing -> (iMap, net)
    Just (Modulator v) -> income targetForwardNeuronIncome p s v iMap net

backward :: Pos -> Signal -> (NeuronIncomeMap, Net) -> (NeuronIncomeMap, Net)
backward p@(x, y) s (iMap, net) = case M.lookup p net of
    Nothing -> (iMap, net)
    Just (Modulator v) -> income targetBackwardNeuronIncome p s v iMap net

remodulate :: Pos -> ModulatorIncome -> (NeuronIncomeMap, Net) -> (NeuronIncomeMap, Net)
remodulate p@(x, y) (s1, s2) d@(iMap, net) = case M.lookup p net of
    Nothing -> (iMap, net)
    Just (Modulator v) | (v < maxModValue) && (s1 > s2)  -> increaseAndBackward v (s1 - s2)
                       | (v < maxModValue) && (s1 < s2)  -> increaseAndForward  v (s2 - s1)
                       | (v < maxModValue) && (s1 == s2) -> increase v
                       | s1 > s2  -> income targetBackwardNeuronIncome p (s1 - s2) v iMap net
                       | s1 < s2  -> income targetForwardNeuronIncome  p (s2 - s1) v iMap net
                       | s1 == s2 -> d
  where
  -- Use old value of modulator!!
    increaseAndBackward v s = let newNet = M.insert p (Modulator $ v+1) net
                              in income targetBackwardNeuronIncome p s v iMap newNet
    increaseAndForward  v s = let newNet = M.insert p (Modulator $ v+1) net
                              in income targetForwardNeuronIncome p s v iMap newNet
    increase v = (iMap, M.insert p (Modulator $ v+1) net)

conductor :: Pos -> ModulatorIncome -> (NeuronIncomeMap, Net) -> (NeuronIncomeMap, Net)
conductor p c@(s, 0) a = forward    p s a
conductor p c@(0, s) a = backward   p s a
conductor p c        a = remodulate p c a

merger :: Pos -> NeuronIncome -> Net -> Net
merger p i = M.adjust (neuronAdjuster i) p
  where
    neuronAdjuster (s1, s2, s3, s4) (Modulator _) = error $ "Unexpected modulator in NeuronIncomeMap at " ++ show p
    neuronAdjuster (s1, s2, s3, s4) (Neuron e)    = Neuron $ neuronSaturation $ e + s1 + s2 + s3 + s4

mergeNeuronSignals :: NeuronIncomeMap -> Net -> Net
mergeNeuronSignals nIMap net = M.foldrWithKey merger net nIMap

decreaseInactiveModulators :: ModulatorIncomeMap -> Net -> Net
decreaseInactiveModulators mIMap = M.mapWithKey deactivator
  where
    deactivator p c@(Neuron _) = c
    deactivator p c@(Modulator v) | p `M.member` mIMap = c
                                  | otherwise = Modulator (max (v-1) minModValue)

emitter :: ModulatorIncomeMap -> Pos -> Cell -> (ModulatorIncomeMap, Cell)
emitter iMap p@(x, y) c@(Modulator _) = (iMap, c)
emitter iMap p@(x, y) c@(Neuron e) | e < netRank = (iMap, c)
                                   | otherwise = (newIMap, Neuron restOfEnergy)
  where
    signal = e `div` netRank
    restOfEnergy = e - (netRank*signal)
    neighbourModulators = [ ((x-1, y), (0, signal)), ((x+1, y), (signal, 0))
                          , ((x, y-1), (0, signal)), ((x, y+1), (signal, 0))]
    newIMap = foldr updateIncome iMap neighbourModulators

-- Can be optimized: remember what neurons can emit.
emitSignals :: Net -> (ModulatorIncomeMap, Net)
emitSignals = M.mapAccumWithKey emitter M.empty

conductSignals :: (ModulatorIncomeMap, Net) -> (ModulatorIncomeMap, (NeuronIncomeMap, Net))
conductSignals (iMap, net) = (iMap, M.foldrWithKey conductor (M.empty, net) iMap)

consumeSignals :: (ModulatorIncomeMap, (NeuronIncomeMap, Net)) -> Net
consumeSignals (mIMap, (nIMap, net)) = decreaseInactiveModulators mIMap . mergeNeuronSignals nIMap $ net

stepNet = consumeSignals . conductSignals . emitSignals


type PosSet = S.Set Pos
data EmitableNeurons = TryAll
                     | Emitable PosSet
  deriving (Show, Read, Eq)

type NetAge = Int
data FastNet = FastNet NetAge EmitableNeurons Net
  deriving (Show, Read, Eq)

selectiveEmitter :: Pos -> (ModulatorIncomeMap, Net) -> (ModulatorIncomeMap, Net)
selectiveEmitter p (mIMap, net) = case M.lookup p net of
    Nothing     -> error $ "Unexpected pos in emitableNeurons at " ++ show p
    Just neuron -> let (newIMap, newNeuron) = emitter mIMap p neuron
                   in  (newIMap, M.insert p newNeuron net)

merger' :: Pos -> NeuronIncome -> (PosSet, Net) -> (PosSet, Net)
merger' p i@(s1, s2, s3, s4) (posSet, net) = result
  where
    result = case M.lookup p net of
        Nothing -> (posSet, net)
        Just (Modulator _) -> error $ "Unexpected modulator in NeuronIncomeMap at " ++ show p
        Just (Neuron e) | e + s1 + s2 + s3 + s4 >= netRank -> (S.insert p posSet, adjustedNeuron e)
                        | otherwise                        -> (posSet,            adjustedNeuron e)
    adjustedNeuron e = M.insert p (Neuron (neuronSaturation $ e + s1 + s2 + s3 + s4)) net

mergeNeuronSignals' :: NeuronIncomeMap -> Net -> (PosSet, Net)
mergeNeuronSignals' nIMap net = M.foldrWithKey merger' (S.empty, net) nIMap

consumeSignals' :: (ModulatorIncomeMap, (NeuronIncomeMap, Net)) -> (EmitableNeurons, Net)
consumeSignals' (mIMap, (nIMap, net)) = let
    (emitablePosSet, net') = mergeNeuronSignals' nIMap net
    in (Emitable emitablePosSet, decreaseInactiveModulators mIMap net')

emitSignals' :: PosSet -> Net -> (ModulatorIncomeMap, Net)
emitSignals' pSet net = S.foldr' selectiveEmitter (M.empty, net) pSet

stepFastNet :: FastNet -> FastNet
stepFastNet fastNet = toFastNet fastNet (f fastNet)
  where
    f (FastNet _ TryAll net)             = consumeSignals' . conductSignals . emitSignals $ net
    f (FastNet _ (Emitable neurons) net) = consumeSignals' . conductSignals . emitSignals' neurons $ net
    toFastNet (FastNet n _ _) (emitable, net) = makeFastNet (n + 1) emitable net


makeFastNet = FastNet
makeStartingFastNet = makeFastNet 0 TryAll
fromFastNet (FastNet _ _ net) = net

-- Tests

testNet = makeRandomNet (20, 20) ((1,1), (20,20)) 100
testNet2 = makeRandomNet (100, 100) ((40,40), (60,60)) 100
steppedTestNet = iterate stepNet (snd testNet2)
viewNet n = head . drop n $ steppedTestNet 

viewMaxFrom n f = M.fold maxNeuron (Neuron 0) (f n)
  where
    maxNeuron (Modulator _) n = n
    maxNeuron (Neuron e1) (Neuron e2) = Neuron (max e1 e2)

viewMax n = viewMaxFrom n viewNet

steppedFastTestNet = let
    net0 = FastNet 0 TryAll (snd testNet2)
    in iterate stepFastNet net0

viewFastNet n = head . drop n $ steppedFastTestNet
viewFastNetMax n = viewMaxFrom n (fromFastNet . viewFastNet)


testEquality n = fromFastNet (viewFastNet n) == viewNet n

