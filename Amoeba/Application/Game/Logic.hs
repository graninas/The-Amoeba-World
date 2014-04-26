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
import Data.Maybe (fromMaybe)

type GameWire a b = GWire GameStateTIO a b

data Screen = Screen1 | Screen2 | Screen3 | Screen4
  deriving (Ord, Eq, Show, Enum)

data Command = Finish
             | Render
             | SwitchScreen Screen
  deriving (Ord, Eq, Show)
  
logic :: GameWire () ()
logic = screen Screen1

screen :: Screen -> GameWire () ()
screen scr = modes Render (selector scr) .
            (
                pure () &&& now . interpreter scr . pollSdlEvent
            )

selector scr Finish = quit . diagnose "Finish"
selector scr Render = mkEmpty . render . diagnose "Render" --> screen scr
selector scr (SwitchScreen swScr) = mkEmpty . (diagnose $ "Switching to: " ++ show swScr) --> screen swScr

interpreter :: Screen -> GameWire SDL.Event Command
interpreter scr = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    SDL.MouseButtonDown{} -> SwitchScreen (next scr)
    _ -> Render

next Screen4 = Screen1
next scr = succ scr
    
diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m

pollSdlEvent :: GameWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        return $ Right e

-- TODO: make it safe on a type-level. Either or Maybe is needed.
render :: GameWire a ()
render = mkGen_ $ \_ -> do
    surf <- getSurface
    withLogError (clearScreen surf) "clearScreen failed."
    w <- getWorld
    withIO $ renderWorld surf w
    withIO $ SDL.flip surf
