module Application.Game.RuntimeSt where

import GameLogic.Data.Facade

class Monad m => RuntimeSt m where
  getData :: m Game
  putData :: Game -> m ()