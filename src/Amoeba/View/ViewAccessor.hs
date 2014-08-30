module Amoeba.View.ViewAccessor where

-- This module should be in View project. Will be moved later.

import Amoeba.View.Language as V
import Amoeba.View.View as V
import Amoeba.View.Runtime as V
import Amoeba.GameLogic.GameLogicAccessor as GL

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM as STM

eval :: (GL.GameLogicAccessor, V.View) -> V.Command -> IO ()
eval (glAccessor, view) V.Render = do
    game <- STM.atomically $ STM.readTVar glAccessor
    V.renderGame view game

