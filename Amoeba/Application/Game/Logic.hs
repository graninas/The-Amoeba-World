module Application.Game.Logic where

import Application.Game.Engine.Types
import Application.Game.Engine.Core
import Application.Game.Runtime
import View.View

import Middleware.FRP.NetwireFacade as FRP
import qualified Control.Wire.Unsafe.Event as E
import Prelude hiding (id, (.))

import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State
import qualified Control.Monad as M (when)
import Control.Concurrent.MVar
import Data.Maybe (fromMaybe)

type GameWire a b = GWire GameStateTIO a b

data Modes = Mode1 | Mode2 | Mode3 | Mode4
  deriving (Ord, Eq, Show)

data Command = Continue | Finish
  deriving (Ord, Eq, Show)

logic :: GameWire () ()
logic = pollSdlEvent --> modes Mode1 selector . 
            (
                pure () &&& interpret . eSdlEvent
            )

selector Mode1 = logic . diagnose "Mode1"
selector Mode2 = logic . diagnose "Mode2"
selector Mode3 = logic . diagnose "Mode3"
selector Mode4 = quit . diagnose "Mode3"

interpret :: GameWire (Event SDL.Event) (Event Modes)
interpret = mkPure_ $ \event -> Right $ fmap interpreter event

interpreter SDL.Quit = Mode4
interpreter SDL.NoEvent = Mode1
interpreter (SDL.LostFocus _) = Mode2
interpreter (SDL.GotFocus _) = Mode2
interpreter SDL.MouseMotion{} = Mode2
interpreter SDL.MouseButtonDown{} = Mode4
interpreter SDL.MouseButtonUp{} = Mode4
interpreter (SDL.KeyDown _) = Mode2
interpreter (SDL.KeyUp _) = Mode2

eSdlEvent :: GameWire () (Event SDL.Event)
eSdlEvent = now . sdlEvent

sdlEvent :: GameWire () SDL.Event
sdlEvent = mkGen_ $ \_ -> do
--    liftIO $ putStrLn "Trying to get event..."
    mbEvent <- tryGetEventFromStore
    -- liftIO $ putStrLn $ "Event got: " ++ show mbEvent
    return $ Right $ fromMaybe SDL.NoEvent mbEvent

trace :: GameWire (Event Modes) (Event Modes)
trace = mkGen_ $ \event -> case event of
    E.NoEvent -> return . Left $ "No netwire event"
    E.Event e -> do
        liftIO $ putStrLn $ "Event: " ++ show e
        return . Right . E.Event $ e

diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m

pollSdlEvent :: GameWire () ()
pollSdlEvent = mkGen_ $ \_ -> do
--    liftIO $ putStrLn "Trying to check if event store is empty..."
    isEmpty <- isEmptyEventStore
--    liftIO $ putStrLn $ "Event store is: " ++ show isEmpty
    if isEmpty
        then do e <- liftIO SDL.pollEvent
                liftIO $ putStrLn $ "SDL event got: " ++ show e
                if (e /= SDL.NoEvent) then (putEventToStore e) >> (return $ Right ())
                                      else return $ Left "SDL event is 'NoEvent' - skipped."
        else return $ Left "Is not ready to take a new event yet." -- TODO: need to store a bunch of events at one time.

processSdlEvent :: GameWire SDL.Event ()
processSdlEvent = mkPure_ $ \event -> case event of
    SDL.Quit -> Left "Quit event: Finished."
    SDL.NoEvent -> Right ()
    SDL.LostFocus _ -> Right ()
    SDL.GotFocus _ -> Right ()
    SDL.MouseMotion{} -> Right ()
    SDL.MouseButtonDown{} -> Right ()
    SDL.MouseButtonUp{} -> Right ()
    SDL.KeyDown _ -> Right ()
    SDL.KeyUp _ -> Right ()
    e -> Left $ "Event not supported: " ++ show e

-- TODO: make it safe in a type-level. Either or Maybe is needed.
render :: GameWire a ()
render = mkGen_ $ \_ -> do
    surf <- getSurface
    withLogError (clearScreen surf) "clearScreen failed."
    w <- getWorld
    withIO $ renderWorld surf w
    withIO $ SDL.flip surf
