module Amoeba.GameLogic.GameLogicAccessor where

import Amoeba.GameLogic.Data.Facade as GL
import Amoeba.GameLogic.Interpreter.Language as I
import Amoeba.GameLogic.Runtime as Rt
import qualified Amoeba.Middleware.Tracing.Log as Log

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM as STM
import Control.Monad (unless, when)
import Control.Concurrent as C
import Control.Applicative ((<$>), (<*>))

type GameLogicAccessor = STM.TVar GameRuntime

-- This function should be refactored in the future.
runGame :: [C.ThreadId] -> GameLogicAccessor -> IO ()
runGame tr glAccessor = do
--  TODO: is it possible to do this simpler?
    res <- STM.atomically $ do
        isInit <- Rt.isInitialising <$> STM.readTVar glAccessor
        when isInit $ STM.modifyTVar' glAccessor (startRunning tr)
        return isInit
    
    Log.info $ "Is initializing state:" ++ show res
    st <- STM.atomically $ Rt.getState <$> STM.readTVar glAccessor
    Log.info $ "Now, state is: " ++ show st
    
    when res $ STM.atomically $
        STM.readTVar glAccessor >>=
            \result -> unless (Rt.isFinished result) STM.retry
    unless res $ Log.error "Failed to initialize game running."

-- HACK to hold main thread's TVar monitoring until heartbeat mechanism is implemented.
--holdGame :: GameLogicAccessor -> IO ()
--holdGame glAccessor = STM.atomically $ STM.modifyTVar' glAccessor setRunning

eval :: GameLogicAccessor -> I.Command -> IO ()
eval glAccessor I.FinishGame = STM.atomically $ STM.modifyTVar' glAccessor Rt.beginFinishing

eval _ glCommand = error $ "eval: game logic command missing: " ++ show glCommand

readGame :: GameLogicAccessor -> IO Game
readGame glAccessor = STM.atomically $ fmap grtGame (STM.readTVar glAccessor)

createGameLogicAccessor :: Game -> IO GameLogicAccessor
createGameLogicAccessor = STM.newTVarIO . initialGameRuntime
