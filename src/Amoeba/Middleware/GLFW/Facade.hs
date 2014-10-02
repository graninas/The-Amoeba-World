module Amoeba.Middleware.GLFW.Facade (module GLFW, errorCallback, withEnvironment) where

import qualified Amoeba.Middleware.Tracing.Log as Log
import Amoeba.Middleware.Tracing.ErrorHandling

import Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless)

errorCallback :: GLFW.ErrorCallback
errorCallback err msg = Log.error $ "GLFW: " ++ show err ++ ", " ++ msg


withEnvironment action = do
    GLFW.setErrorCallback $ Just errorCallback
    Log.info "GLFW: error callback set."
    success <- GLFW.init
    when success $ do
        Log.info "GLFW: initialization succeeded."
        action
    unless success $ Log.info "GLFW: initialization failed."
