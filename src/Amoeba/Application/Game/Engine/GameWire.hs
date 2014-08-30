module Amoeba.Application.Game.Engine.GameWire where

import Amoeba.Middleware.FRP.NetwireFacade
import Amoeba.Application.Game.Engine.Runtime

type Inhibitor = String
type GTime = Timed NominalDiffTime ()
type GWire state a b = Wire GTime Inhibitor state a b


-- TODO: HACK: FIXME: needs deep generalization of wires mechanism.
-- Will do it later. Now, this is a HACK and copy-paste.

type ViewWire a b        = GWire ViewTIO a b
type GameStorageWire a b = GWire GameStorageTIO a b
type AIPlayerWire a b    = GWire AIPlayerTIO a b