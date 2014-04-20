module Application.Game.Logic where

import Application.Game.Engine.Types
import Application.Game.Engine.Core
import Application.Game.Runtime
import View.View

import Middleware.FRP.NetwireFacade as FRP
import Prelude hiding (id, (.))

import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State

type GameWire a b = GWire GameStateTIO a b

data Modes = Mode1 | Mode2 | Mode3 | Finisher
  deriving (Ord, Eq)

logic :: GameWire () ()
logic = modes Mode1 selector .
    (
       diagnose "Some data" &&& (interpret . eSdlEvent --> now . pure Mode3)
    )

selector Mode1 = diagnose "Mode1"
selector Mode2 = diagnose "Mode2"
selector Mode3 = diagnose "Mode3"
selector Finisher = quit

interpret :: GameWire (Event SDL.Event) (Event Modes)
interpret = mkPure_ $ \event -> Right $ fmap interpreter event


interpreter SDL.Quit = Finisher
interpreter SDL.MouseButtonUp{} = Mode2
interpreter SDL.MouseMotion{} = Mode2
interpreter SDL.MouseButtonDown{} = Mode2
interpreter _ = Mode1

eSdlEvent :: GameWire () (Event SDL.Event)
eSdlEvent = now . pollSdlEvent




diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m

pollSdlEvent :: GameWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
    e <- liftIO SDL.pollEvent
    liftIO $ print e 
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
