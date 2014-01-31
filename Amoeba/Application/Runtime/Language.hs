module Application.Runtime.Language where

import Control.Wire
import Control.Monad.State
import Prelude hiding ((.), id)

import View.View
import Middleware.Config.Facade


data Runtime = Runtime { rtConfiguration :: Configuration
                       , rtView :: View
                       }
                       

type WStateIO = StateT Runtime IO
type WWire a b = Wire (Timed NominalDiffTime ()) () IO a b


runtime = Runtime