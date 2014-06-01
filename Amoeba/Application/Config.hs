module Application.Config where

import Middleware.Config.Facade as Cfg
import Application.Assets.ConfigScheme as Scheme

logFileLoader = Cfg.filePathLoader Scheme.logPath "Amoeba.log"