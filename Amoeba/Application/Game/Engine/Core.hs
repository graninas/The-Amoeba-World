module Application.Game.Engine.Core where

import Middleware.FRP.NetwireFacade

quit = mkConst $ Left "Finished."