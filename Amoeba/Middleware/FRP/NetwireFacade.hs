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
    , (W.-->)
    , W.asSoonAs
    , W.now
    , W.at
    ) where

import qualified Control.Wire as W
