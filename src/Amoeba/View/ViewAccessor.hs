module Amoeba.View.ViewAccessor (
      module Accessor
    , initView
    , deinitView
    , evalViewCommand
    , getEvent
    , ViewAccessor(..)
    ) where

import Amoeba.View.Language
import Amoeba.View.Input.InputAccessor as Accessor
import Amoeba.View.Output.OutputAccessor as Accessor
import Amoeba.View.Output.Types
import qualified Amoeba.GameLogic.GameLogicAccessor as GL

import qualified Amoeba.Middleware.OpenGL.Facade as OGL
import qualified Amoeba.Middleware.GLFW.Facade as GLFW
import qualified Amoeba.Middleware.Tracing.Log as Log

import Control.Concurrent as C
import Control.Concurrent.STM.TVar as STM
import Control.Concurrent.STM as STM (atomically)
import System.Exit

data ViewAccessor = ViewAccessor Accessor.InputAccessor Accessor.OutputAccessor
mkViewAccessor = ViewAccessor

getInputAccessor  (ViewAccessor ia _) = ia
getOutputAccessor (ViewAccessor _ oa) = oa

initView :: (Screen, String, UserViewPoint) -> IO ViewAccessor
initView (Screen w h _, caption, viewPlane) = do
    mw <- GLFW.createWindow w h caption Nothing Nothing
    case mw of
        Nothing -> error "GLFW: Create window failed."  -- This looks wrong
        Just window -> do
            Log.info "Window created."
            GLFW.makeContextCurrent mw
            GLFW.swapBuffers window
            Log.info "Context made current."
            outputAccessor <- initOutputAccessor window viewPlane
            inputAccessor  <- initInputAccessor window
            Log.info "Accessors created."
            return $ mkViewAccessor inputAccessor outputAccessor

deinitView :: ViewAccessor -> IO ()
deinitView (ViewAccessor _ oa) = getWindow oa >>= GLFW.destroyWindow

getEvent :: ViewAccessor -> IO Event
getEvent = tryReadEvent . getInputAccessor

evalViewCommand :: (GL.GameLogicAccessor, ViewAccessor) -> Command -> IO ()
evalViewCommand (glAccessor, viewAccessor) Render = do
    --game <- readGame glAccessor
    --view <- readView viewAccessor
    --renderGame view game
    return ()
evalViewCommand (glAccessor, viewAccessor) command = do
    --game <- STM.atomically $ STM.readTVar glAccessor
    --STM.atomically $ STM.modifyTVar' viewAccessor (modifyView game command)
    return ()

