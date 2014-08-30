module Amoeba.Application.Assets.ViewFlow where

import Amoeba.View.Language
import Amoeba.Application.Assets.Wires
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.Engine.Core

import Amoeba.Middleware.FRP.NetwireFacade as FRP

import Prelude hiding (id, (.))

viewFlow :: GameNode -> ViewWire () ()
viewFlow node = modes Render (selector node) .
            (
                pure () &&& now . commandInterpreter node . pollSdlEvent
            )

switcher node w1 = mkEmpty . w1 --> viewFlow node

selector node Finish      = quit . finishGame . diagnose "Manually finished."
selector node Render      = switcher node  render
selector node viewCommand = switcher node (render . (evalViewCommand viewCommand))

