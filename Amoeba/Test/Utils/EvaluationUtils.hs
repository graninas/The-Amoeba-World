module Test.Utils.EvaluationUtils where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import System.Random
import Control.Lens
import Control.Monad.State
import Control.Monad

import Test.Utils.TestGameData

import GameLogic.World
import GameLogic.Player
import GameLogic.Geometry
import GameLogic.Objects
import GameLogic.Object
import GameLogic.Scenario
import GameLogic.Evaluation
import GameLogic.Game
import GameLogic.AI as AI
import qualified GameLogic.GenericAI as GAI

nextRnd' :: Game -> Eval Int
nextRnd' game = let (r, g) = random (game ^. rndGen)
                    newGame = rndGen .~ g $ game
               in do
                   ctx <- get
                   put $ ctx { _ctxNextRndNum = nextRnd' newGame }
                   return r

getObjectAt' :: Game -> Point -> Eval (Maybe Object)
getObjectAt' game p = return $ game ^? objects . ix p

getObjects' :: Game -> Eval Objects
getObjects' game = return $ fromMap (game ^. objects)

getObjectGraph' :: Game -> Eval (NeighboursFunc -> ObjectGraph)
getObjectGraph' game = return $ GAI.graph (game ^. world)

testContext :: Game -> EvaluationContext
testContext game = context dataCtx rndF
  where
    rndF = nextRnd' game
    dataCtx = dataContext objectsF objectGraphF objectAtF
    objectGraphF = getObjectGraph' game
    objectAtF = getObjectAt' game
    objectsF = getObjects' game

testGameAndContext seed = let
    game = testGame seed
    ctx = testContext game
    in (game, ctx)
