module Amoeba.Application.Config where

import Amoeba.Middleware.Config.Facade as Cfg
import Amoeba.Assets.ConfigScheme as Scheme


logFileLoader = Cfg.filePathLoader Scheme.logPath "Amoeba.log"
worldFileLoader = Cfg.filePathLoader Scheme.rawsPath "World.arf"