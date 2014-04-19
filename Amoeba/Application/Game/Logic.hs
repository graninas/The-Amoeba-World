module Application.Game.Logic where

import Application.Game.Engine.Types
import Application.Game.Engine.Core
import Application.Game.Runtime
import View.Color
import View.View

import Middleware.FRP.NetwireFacade
import Prelude hiding (id, (.))

import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State

type GameWire a b = GWire GameStateTIO a b

logic :: GameWire () ()
--logic = (for 2 . diagnose "1") --> for 3 . diagnose "2" --> quit
logic = holdFor 0.1 . periodic 1 . diagnose "1" --> for 3 . diagnose "2" --> logic

diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m

pollEventWire :: GameWire () SDL.Event
pollEventWire = mkGen_ $ \_ -> do
    e <- liftIO SDL.pollEvent
    return $ Right e

eventMapperWire :: GameWire SDL.Event ()
eventMapperWire = mkPure_ $ \event -> case event of
    SDL.NoEvent -> Right ()
    SDL.Quit -> Left "Quit event: Finished."
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
