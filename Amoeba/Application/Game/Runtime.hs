module Application.Game.Runtime where

import View.View
import GameLogic.Data.Facade
import Middleware.Config.Facade
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State (get, modify, put, StateT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Control.Concurrent.MVar

data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtGame :: Game
                     }

type GameStateTIO = StateT GameRt IO

runtime = GameRt

getWorld :: GameStateTIO World
getWorld = liftM (gWorld . grtGame) get

getSurface :: GameStateTIO ViewSurface
getSurface = liftM (viewSurface . grtView) get

getEventStore :: GameStateTIO EventStore
getEventStore = liftM (viewEventStore . grtView) get


isEmptyEventStore :: GameStateTIO Bool
isEmptyEventStore = do
    store <- getEventStore
    liftIO $ isEmptyMVar store

-- TODO: use lens to update it.
putEventToStore :: SDL.Event -> GameStateTIO ()
putEventToStore event = do
    store <- getEventStore
    liftIO $ putMVar store event
    rt@(GameRt _ view _) <- get
    let view' = view { viewEventStore = store }
    let rt' = rt { grtView = view' }
    put rt'

tryGetEventFromStore :: GameStateTIO (Maybe SDL.Event)
tryGetEventFromStore = do
    store <- getEventStore
    liftIO $ tryTakeMVar store

getEventFromStore :: GameStateTIO SDL.Event
getEventFromStore = do
    store <- getEventStore
    liftIO $ takeMVar store