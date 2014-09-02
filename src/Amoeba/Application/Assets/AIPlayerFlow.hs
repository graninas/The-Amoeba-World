module Amoeba.Application.Assets.AIPlayerFlow where

--import Amoeba.Application.Assets.Wires
import Amoeba.Application.Game.Engine.GameWire

import Amoeba.Middleware.FRP.NetwireFacade as FRP
--import qualified Amoeba.Middleware.Tracing.Log as Log

--import Control.Arrow
import Prelude hiding ((.), id)

aiPlayerFlow :: AIPlayerWire () ()
aiPlayerFlow = FRP.mkConst $ Right ()

