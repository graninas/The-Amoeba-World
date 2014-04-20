module Application.Game.Logic where

import Application.Game.Engine.Types
import Application.Game.Engine.Core
import Application.Game.Runtime
import View.View

import Middleware.FRP.NetwireFacade
import Prelude hiding (id, (.))

import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State

type GameWire a b = GWire GameStateTIO a b

logic :: GameWire () ()
logic = modes True selector . 
    (
        (diagnose "left" &&& for 2 . now . pure True)
        --> (diagnose "right" &&& for 3 . now . pure False)
    )

selector True = diagnose "selector True"
selector False = quit . diagnose "selector False"

test = for 1 . diagnose "1" 
        --> for 1 . processSdlEvent . pollSdlEvent
        --> for 2 . diagnose "2"
        --> logic

diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m


pollSdlEvent :: GameWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
    liftIO $ putStrLn "Polling..."
    e <- liftIO SDL.pollEvent
    return $ Right e

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
