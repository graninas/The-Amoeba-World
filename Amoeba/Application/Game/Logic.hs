{-# LANGUAGE BangPatterns #-}

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

type GameWire a b = GWire GameStateTIO a b

data Modes = Mode1 | Mode2 | Mode3
  deriving (Ord, Eq, Show)

data Command = Continue | Finish
  deriving (Ord, Eq, Show)

logic :: GameWire () ()
logic = commandInterpreter . inputInterpreter


inputInterpreter = for 0.016 . modes Mode1 selector . 
            (
               nop &&& interpret . eSdlEvent
            ) -- --> pure Continue

commandInterpreter = mkGen_ $ \command -> withIO $ print command

nop = pure ()

selector Mode1 = pure Continue
selector Mode2 = pure Continue
selector Mode3 = pure Finish

interpret :: GameWire (Event SDL.Event) (Event Modes)
interpret = mkPure_ $ \event -> Right $ fmap interpreter event

trace :: GameWire (Event Modes) (Event Modes)
trace = mkGen_ $ \event -> case event of
    E.NoEvent -> return . Left $ "No netwire event"
    E.Event e -> do
        liftIO $ putStrLn $ "Event: " ++ show e
        return . Right . E.Event $ e
    

interpreter SDL.Quit = Mode3
interpreter SDL.NoEvent = Mode1
interpreter (SDL.LostFocus _) = Mode2
interpreter (SDL.GotFocus _) = Mode2
interpreter SDL.MouseMotion{} = Mode2
interpreter SDL.MouseButtonDown{} = Mode2
interpreter SDL.MouseButtonUp{} = Mode2
interpreter (SDL.KeyDown _) = Mode2
interpreter (SDL.KeyUp _) = Mode2


eSdlEvent :: GameWire () (Event SDL.Event)
eSdlEvent = now . pollSdlEvent

diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m

pollSdlEvent :: GameWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
    e <- liftIO SDL.pollEvent
    liftIO $ putStrLn $ "SDL event: " ++ show e
    return (if e == SDL.NoEvent
                then Left "SDL: no event."
                else Right e)

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
