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

selector node Finish                     = quit . finishGame . diagnose "Finish"
selector node Render                     = switcher node   render
{-
selector node (StartViewPointMoving x y) = switcher node (render . startViewPointMoving . pure (x, y))
selector node (ViewPointMoving x y)      = switcher node (render . viewPointMoving . pure (x, y))
selector node (StopViewPointMoving x y)  = switcher node (render . stopViewPointMoving . pure (x, y))
selector node Update                     = switcher node (render . update)
-}

