module Amoeba.Application.Boot where

import qualified Amoeba.Middleware.Config.Facade as Cfg
import qualified Amoeba.Middleware.Tracing.Log as Log
import qualified Amoeba.Middleware.GLFW.Facade as GLFW

import Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.Application.Game.Engine.Core as Core
import Amoeba.GameLogic.GameLogicAccessor as GLAcc
import Amoeba.GameLogic.Facade as GL
--import Amoeba.GameStorage.Facade as GS
import Amoeba.View.ViewAccessor as ViewAcc
import Amoeba.View.Facade as V

import Amoeba.Application.Assets.ViewFlow
import Amoeba.Application.Assets.GameStorageFlow
import Amoeba.Application.Assets.AIPlayerFlow
import Amoeba.Application.Assets.ViewConfig

startViewFlow :: GameLogicAccessor -> ViewAccessor -> IO ()
startViewFlow glAccessor viewAccessor = do
    let rt = Rt.mkViewRuntime viewAccessor glAccessor
    (inhibitor, _) <- Core.startMainLoopView viewFlow rt
    Log.info $ "[View] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

boot :: Cfg.Configuration ->  GL.Game -> IO ()
boot cfg game = GLFW.withEnvironment $ do
    
    glAccessor <- GLAcc.createGameLogicAccessor game
    
    viewSettings <- loadViewSettings cfg
    Log.info "View settings loaded."
    viewAccessor <- ViewAcc.initView viewSettings
    Log.info "View prepared."

    Log.info "Polling events..."
    GLFW.pollEvents
    
    Log.info "Starting flow..."
    startViewFlow glAccessor viewAccessor

    Log.info "Deinitializing View..."
    ViewAcc.deinitView viewAccessor
    
    
    