module Middleware.FRP.NetwireFacade 
    ( W.Wire
    , W.Timed
    , W.NominalDiffTime
    , W.clockSession_
    , W.stepSession
    , W.stepWire
    , W.mkPure_
    , W.mkPure
    , W.mkConst
    , W.mkGen_
    , W.mkGen
    , (W.<|>)
    ) where

import qualified Control.Wire as W
