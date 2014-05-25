module CellularNet.Loader where

import Middleware.Config.Config (Configuration)
import CellularNet.Net

load :: Configuration -> IO FastNet
load _ = return $ viewFastNet 0