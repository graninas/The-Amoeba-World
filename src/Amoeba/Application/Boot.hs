module Amoeba.Application.Boot where

import qualified Amoeba.Middleware.Config.Facade as Cfg
import qualified Amoeba.Middleware.Tracing.Log as Log
import qualified Amoeba.Middleware.SDL.Environment as Env
import qualified Amoeba.Middleware.FRP.NetwireFacade as FRP

import Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.Application.Game.Engine.Core as Core
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.GameDataLoader
import Amoeba.Application.Config
import Amoeba.GameLogic.GameLogicAccessor as GLAcc
import Amoeba.GameStorage.Facade as GS
import Amoeba.View.ViewAccessor as ViewAcc
import Amoeba.View.Language
import Amoeba.View.Config
import Amoeba.View.View

import qualified Amoeba.Application.Assets.ViewFlow as VF

import Paths_The_Amoeba_World as P

import Control.Concurrent as C
-- Temp.
import Control.Monad.IO.Class (liftIO)


-- This is temorary functions.
aiPlayerFlow :: AIPlayerWire () ()
aiPlayerFlow = FRP.mkConst $ Right ()

gameStorageFlow :: GameStorageWire () ()
gameStorageFlow = FRP.mkGen_ $ const $ do
    glAccessor <- getStorageGameLogicAccessor
    liftIO $ GLAcc.holdGame glAccessor
    return $ Right () -- TODO: replace by retR() after generalization. Or replace liftIO by withIO.

viewFlow :: ViewWire () ()
viewFlow = VF.viewFlow TitleScreen

-- TODO: needs deep generalization of looping and wire mechanism.

startGameStorageFlow glAccessor = do
    (inhibitor, _) <- Core.startMainLoopGS gameStorageFlow glAccessor
    Log.info $ "[GameStorage] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

startAIPlayerFlow glAccessor = do
    (inhibitor, _) <- Core.startMainLoopAI aiPlayerFlow glAccessor
    Log.info $ "[AI Player] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

startViewFlow glAccessor view = Env.withEnvironment $ do
    let rt = Rt.viewRuntime view glAccessor
    (inhibitor, _) <- Core.startMainLoopView viewFlow rt
    Log.info $ "[View] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

-- Forkers for workers

forkGameStorageWorker game = do
    glAccessor <- initGameStorage game
    Log.info "Game Storage initialized."
    gsThreadId <- C.forkFinally (startGameStorageFlow glAccessor) (\_ -> Log.info "Game Storage thread finished.")
    Log.info $ "Game Storage thread started: " ++ show gsThreadId
    return (glAccessor, gsThreadId)

forkAIPlayerWorker glAccessor = do
    aiPlayerAccessor <- return "This is fake AI Player." -- TODO: make non-fake AI player.
    Log.info "AI loaded."
    aiPlayerThreadId <- C.forkFinally (startAIPlayerFlow glAccessor) (\_ -> Log.info "AI thread finished.")
    Log.info $ "AI player thread started: " ++ show aiPlayerThreadId
    return (aiPlayerAccessor, aiPlayerThreadId)

forkViewWorker cfg glAccessor = do
    viewSettings <- loadViewSettings cfg
    Log.info "View settings loaded."
    viewAccessor <- ViewAcc.initView viewSettings
    Log.info "View prepared."
    viewThreadId <- C.forkFinally (startViewFlow glAccessor viewAccessor) (\_ -> Log.info "View thread finished.")
    Log.info $ "View thread started: " ++ show viewThreadId
    return (viewAccessor, viewThreadId)

boot cfg = do
    logFilePath <- Cfg.extract cfg logFileLoader   >>= P.getDataFileName
    worldPath   <- Cfg.extract cfg worldFileLoader >>= P.getDataFileName
    
    Log.setupLogger logFilePath
    Log.info $ "Logger started: " ++ logFilePath

    game <- loadGame worldPath
    Log.info $ "Game loaded from: " ++ worldPath

    (glAccessor,  gsThreadId)    <- forkGameStorageWorker game
    (aipAccessor, aipThreadId)   <- forkAIPlayerWorker glAccessor
    (viewAccessor, viewThreadId) <- forkViewWorker cfg glAccessor
    
    Log.info "Running...."
    GLAcc.runGame glAccessor

    Log.info "Game unloaded."
    Log.finish
