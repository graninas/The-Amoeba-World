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

import Amoeba.Application.Assets.ViewFlow
import Amoeba.Application.Assets.GameStorageFlow
import Amoeba.Application.Assets.AIPlayerFlow
import Amoeba.Application.Assets.ViewConfig

import Control.Concurrent as C

-- TODO: investigate this function. It seems wrong.
initGameStorage :: GL.Game -> IO GLAcc.GameLogicAccessor
initGameStorage = GLAcc.createGameLogicAccessor

-- TODO: needs deep generalization of looping and wire mechanism.

startGameStorageFlow :: GameLogicAccessor -> IO ()
startGameStorageFlow glAccessor = do
    (inhibitor, _) <- Core.startMainLoopGS gameStorageFlow glAccessor
    Log.info $ "[GameStorage] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

startAIPlayerFlow :: GameLogicAccessor -> IO ()
startAIPlayerFlow glAccessor = do
    (inhibitor, _) <- Core.startMainLoopAI aiPlayerFlow glAccessor
    Log.info $ "[AI Player] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

startViewFlow :: GameLogicAccessor -> ViewAccessor -> IO ()
startViewFlow glAccessor viewAccessor = do
    let rt = Rt.mkViewRuntime viewAccessor glAccessor
    (inhibitor, _) <- Core.startMainLoopView viewFlow rt
    Log.info $ "[View] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

-- Forkers for workers

forkGameStorageWorker :: GL.Game -> IO (GameLogicAccessor, C.ThreadId)
forkGameStorageWorker game = do
    glAccessor <- initGameStorage game
    Log.info "Game Storage initialized."
    gsThreadId <- C.forkFinally (startGameStorageFlow glAccessor) (\_ -> Log.info "Game Storage thread finished.")
    Log.info $ "Game Storage thread started: " ++ show gsThreadId
    return (glAccessor, gsThreadId)

forkAIPlayerWorker :: GameLogicAccessor -> IO (String, C.ThreadId)
forkAIPlayerWorker glAccessor = do
    let aiPlayerAccessor = "This is fake AI Player." -- TODO: make non-fake AI player.
    Log.info "AI loaded."
    aiPlayerThreadId <- C.forkFinally (startAIPlayerFlow glAccessor) (\_ -> Log.info "AI thread finished.")
    Log.info $ "AI player thread started: " ++ show aiPlayerThreadId
    return (aiPlayerAccessor, aiPlayerThreadId)

-- TODO: refactor it. Possibly, introduce the Accessor type class. 
forkViewWorker :: Cfg.Configuration -> GameLogicAccessor -> IO (ViewAccessor, C.ThreadId)
forkViewWorker cfg glAccessor = do
    viewSettings <- loadViewSettings cfg
    Log.info "View settings loaded."
    viewAccessor <- ViewAcc.initView viewSettings
    Log.info "View prepared."
    viewThreadId <- C.forkFinally (startViewFlow glAccessor viewAccessor)
                                  (\_ -> do
                                        ViewAcc.deinitView viewAccessor
                                        Log.info "View thread finished.")
    Log.info $ "View thread started: " ++ show viewThreadId
    return (viewAccessor, viewThreadId)
    
boot :: Cfg.Configuration ->  GL.Game -> IO ()
boot cfg game = GLFW.withEnvironment $ do
    Log.info "Forking work threads...."

    (glAccessor,   gsThreadId)   <- forkGameStorageWorker game
    (aiAccessor,   aipThreadId)  <- forkAIPlayerWorker glAccessor
    (viewAccessor, viewThreadId) <- forkViewWorker cfg glAccessor
    let threads = [gsThreadId, aipThreadId, viewThreadId]
    
    Log.info "Running...."
    
    GLAcc.runGame threads glAccessor

    -- TODO: Terminate threads!

