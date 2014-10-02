module Amoeba.GameLogic.Interpreter.Language where

import Amoeba.GameLogic.Data.Facade

data Command = FinishGame
             | AddStrategy Player Strategy
  deriving (Show, Read, Eq)