module Application.Game.Runtime where

import View.View
import GameLogic.Data.Facade
import Middleware.Config.Facade

data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtGame :: Game
                     }

runtime = GameRt

