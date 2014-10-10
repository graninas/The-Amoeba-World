module Amoeba.View.Output.OutputAccessor where

import qualified Amoeba.Middleware.GLFW.Facade as GLFW

import Amoeba.View.Output.Types
import Amoeba.View.Output.Runtime
import Amoeba.Middleware.OpenGL.Facade as OGL

import Control.Concurrent.STM.TVar as STM
import Control.Concurrent.STM as STM

type OutputAccessor = STM.TVar Runtime

initOutputAccessor :: GLFW.Window -> UserViewPoint -> IO OutputAccessor
initOutputAccessor window virtualPlane = do
    shadeModel $= Smooth
    lineSmooth $= Enabled -- antialiasing
    blend      $= Enabled
    blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
    clearColor $= black
    
    STM.newTVarIO $ Runtime window virtualPlane Nothing

getWindow :: OutputAccessor -> IO GLFW.Window
getWindow = fmap runtimeWindow . atomically . readTVar

-- From here: https://github.com/crockeo/netwire-pong/blob/master/src/Main.hs
-- Because I don't know the OpenGL.
adjustWindowSize :: ViewPointCoordinates -> IO ()
adjustWindowSize (w', h') = do
    let (w, h) = ( (fromIntegral w' / 640) * 100    -- Strange magic constants.
                 , (fromIntegral h' / 640) * 100    -- TODO: investigate.
                 )

    matrixMode $= Projection
    loadIdentity
    ortho (-w) ( w)
          (-h) ( h)
          (-1) ( 1)

    matrixMode $= Modelview 0
    viewport $= (Position 0 0, Size (fromIntegral w') (fromIntegral h'))
    