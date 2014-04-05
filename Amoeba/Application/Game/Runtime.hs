module Application.Game.Runtime where

import GameLogic.Data.Facade
import View.View
import Middleware.Config.Facade

data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtGame :: Game
                     }

runtime = GameRt

