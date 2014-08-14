module Application.Assets.GameFlow1 where

import View.Language
import Application.Assets.Wires
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core

import Middleware.FRP.NetwireFacade as FRP

import Prelude hiding (id, (.))

gameNode :: GameNode -> GameWire () ()
gameNode node = modes Render (selector node) .
            (
                pure () &&& now . commandInterpreter node . pollSdlEvent
            )

switcher node w1 = mkEmpty . w1 --> gameNode node

selector _     Finish                    = quit . diagnose "Finish"
selector node  Render                    = switcher node render
selector _    (SwitchNode swNode)        = switcher swNode render
selector node (StartViewPointMoving x y) = switcher node (render . startViewPointMoving . pure (x, y))
selector node (ViewPointMoving x y)      = switcher node (render . viewPointMoving . pure (x, y))
selector node (StopViewPointMoving x y)  = switcher node (render . stopViewPointMoving . pure (x, y))
selector node  Update                    = switcher node (render . update)

