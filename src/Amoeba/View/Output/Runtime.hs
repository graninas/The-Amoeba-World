module Amoeba.View.Output.Runtime where

import Amoeba.View.Output.Types

import qualified Amoeba.Middleware.GLFW.Facade as GLFW

data Runtime = Runtime
    { runtimeWindow :: GLFW.Window
    , runtimeUserViewPoint :: UserViewPoint
    , runtimeMouseDrag :: Maybe (ViewPointCoordinates, ViewPointCoordinates)
    }
