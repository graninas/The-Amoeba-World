module Amoeba.View.Output.OutputAccessor (
      OutputAccessor
    , initOutputAccessor
    , getWindow
    ) where

import Control.Concurrent.STM.TVar as STM
import Control.Concurrent.STM as STM

import qualified Graphics.Rendering.OpenGL as OGL
import qualified Amoeba.Middleware.GLFW.Facade as GLFW

import Amoeba.View.Output.Types
import Amoeba.View.Output.Runtime

type OutputAccessor = STM.TVar Runtime

initOutputAccessor :: GLFW.Window -> UserViewPoint -> IO OutputAccessor
initOutputAccessor window virtualPlane = do
    OGL.shadeModel OGL.$= OGL.Smooth
    -- enable antialiasing
    OGL.lineSmooth OGL.$= OGL.Enabled
    OGL.blend OGL.$= OGL.Enabled
    OGL.blendFunc OGL.$= (OGL.SrcAlpha, OGL.OneMinusSrcAlpha)
    -- set the color to clear background
    OGL.clearColor OGL.$= OGL.Color4 0 0 0 0
    
    STM.newTVarIO $ Runtime window virtualPlane Nothing

getWindow :: OutputAccessor -> IO GLFW.Window
getWindow = fmap runtimeWindow . atomically . readTVar
            
            