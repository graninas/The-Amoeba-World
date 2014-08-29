module Amoeba.Application.Boot where

import Amoeba.View.Config
import Amoeba.View.View

import qualified Amoeba.Middleware.Config.Facade as Cfg
import qualified Amoeba.Middleware.Tracing.Log as Log
import qualified Amoeba.Middleware.SDL.Environment as Env

import Amoeba.Application.Game.Engine.Runtime
import Amoeba.Application.Game.Engine.Core
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.GameDataLoader
import Amoeba.GameStorage.GameStorage
--import Amoeba.AI.Facade as AI

import Amoeba.Application.Config
import Amoeba.Application.Assets.GSStorageFlow
import Amoeba.View.Language

import qualified Amoeba.Application.Assets.GameFlow1 as GF1
import qualified Amoeba.Application.Assets.GameFlow2 as GF2
import qualified Amoeba.Application.Assets.GSStorageFlow as GSF1

import Paths_The_Amoeba_World as P

import Control.Concurrent as C


viewLogic :: GameWire () ()
viewLogic = GF1.gameNode TitleScreen

gameStorageLogic :: GameStorageWire () ()
gameStorageLogic = GSF1.flow

-- TODO: cfg duplicates viewSettings.
-- TODO: needs deep generalization of looping mechanism.
-- 'game' is a rudiment. There is a game storage now. Remove 'game'
startAIViewFlow cfg game view = Env.withEnvironment $ do
    let rt = runtime cfg view game
    (inhibitor, _) <- startMainLoop viewLogic rt
    Log.info $ "[AI Flow] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

startGSFlow gsStore = do
    (inhibitor, _) <- startMainLoop2 gameStorageLogic gsStore
    Log.info $ "[GameStorage] Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor

data AI = AI
loadAI = return AI

-- TODO: add specific AI player settings
-- 'game' is a rudiment. There is a game storage now. Remove 'game'!
forkAIPlayer cfg game = do
    ai <- loadAI
    Log.info "AI loaded."
    aiPlayerViewSettings <- loadViewSettings cfg
    Log.info "AI view settings loaded."
    aiView <- setupView aiPlayerViewSettings
    Log.info "AI view prepared."
    aiViewThreadId <- C.forkFinally (startAIViewFlow cfg game aiView) (\_ -> Log.info "AI view thread finished.")
    Log.info $ "AI player thread started: " ++ show aiViewThreadId
    return (ai, aiViewThreadId)

forkGameStateStorageWorker game = do
    gameStateStorage <- initGameStateStorage game
    Log.info "Game State Storage initialized."
    gssThreadId <- C.forkFinally (startGSFlow gameStateStorage) (\_ -> Log.info "Game State Storage thread finished.")
    Log.info $ "Game State Storage thread started: " ++ show gssThreadId
    return (gameStateStorage, gssThreadId)
    
boot cfg = do
    logFilePath <- Cfg.extract cfg logFileLoader   >>= P.getDataFileName
    worldPath   <- Cfg.extract cfg worldFileLoader >>= P.getDataFileName
    
    Log.setupLogger logFilePath
    Log.info $ "Logger started: " ++ logFilePath

    game <- loadGame worldPath
    Log.info $ "Game loaded from: " ++ worldPath

    (ai, aiPlayerThreadId) <- forkAIPlayer cfg game
    (gsStorage, gssThreadId) <- forkGameStateStorageWorker game
    
    Log.info "Running...."
    waitForEndOfGame gsStorage
    
    -- TODO: correct program termination, wait for threads?
    
    Log.info "Game unloaded."
    Log.finish
    


 