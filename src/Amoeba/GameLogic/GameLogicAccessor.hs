module Amoeba.GameLogic.GameLogicAccessor where

-- This module should be in GameLogic (or possibly in GameStorage) project. Will be moved later.

import Amoeba.GameLogic.Facade as GL
import Amoeba.GameStorage.Facade as GS

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM as STM

-- N.B.: GameLogicAccessor can be another type in the future.
-- For example, this could be a module with it's own thread, which manages the game logic.
type GameLogicAccessor = GS.GameStorageAccessor

-- This function can behave different in the future.
-- For example, it will ask GameLogic mechanics to show user a message 'Are you shure?'.
-- But now it just waits for the 'Finished' game state flag.
runGame :: GameLogicAccessor -> IO ()
runGame gStorage = STM.atomically $
    STM.readTVar gStorage >>= \result ->
        if GL.isGameFinished result
        then return ()
        else STM.retry

-- HACK to hold main thread's TVar monitoring until heartbeat mechanism is implemented.
holdGame :: GS.GameStorageAccessor -> IO ()
holdGame gsAccessor = STM.atomically $ STM.modifyTVar gsAccessor setGameRunning

eval :: GameLogicAccessor -> GL.Command -> IO ()
eval glAccessor GL.FinishGame = STM.atomically $ STM.modifyTVar glAccessor GL.setGameFinished
