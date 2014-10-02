module Amoeba.GameStorage.GameStorage where

import Amoeba.GameLogic.Facade as GL
import Amoeba.GameLogic.GameLogicAccessor as GL

import qualified Control.Concurrent.STM.TVar as STM

-- Seems wrong.
initGameStorage :: GL.Game -> IO GameLogicAccessor
initGameStorage = createGameLogicAccessor

