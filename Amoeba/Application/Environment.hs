module Application.Environment where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF.General as TTF

import Control.Exception (bracket_, bracket)

withTtf = bracket_ TTF.init TTF.quit

withEnvironment action = withTtf (SDL.withInit [SDL.InitEverything] action)

