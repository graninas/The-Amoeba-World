module Amoeba.Application.Assets.GameStorageFlow where

import Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.Application.Game.Engine.GameWire as W

import Amoeba.GameLogic.GameLogicAccessor as GLAcc

import qualified Amoeba.Middleware.FRP.NetwireFacade as FRP


gameStorageFlow :: W.GameStorageWire () ()
gameStorageFlow = FRP.mkGen_ $ const $ do
    glAccessor <- Rt.getStorageGameLogicAccessor
    withIO $ GLAcc.holdGame glAccessor