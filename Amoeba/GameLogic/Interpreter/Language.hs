module GameLogic.Interpreter.Language where

import GameLogic.Data.Facade
import Middleware.Math.Geometry



data Command = AddStrategy Player Strategy
  deriving (Show, Read, Eq)