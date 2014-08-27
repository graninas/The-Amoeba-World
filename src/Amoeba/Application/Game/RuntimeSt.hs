module Amoeba.Application.Game.RuntimeSt where

import Amoeba.GameLogic.Data.Facade

class Monad m => RuntimeSt m where
  getData :: m Game
  putData :: Game -> m ()