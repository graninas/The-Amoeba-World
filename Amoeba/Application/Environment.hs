module Application.Environment where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF.General as TTF

import Control.Exception (bracket_, bracket)

import GameView.View

withTtf = bracket_ TTF.init TTF.quit

withEnvironment action = withTtf (SDL.withInit [SDL.InitEverything] action)

setupScreen :: (Screen, Int) -> String -> IO View
setupScreen (scr@(Screen scrWidth scrHeight), scrBpp) caption = do
    surface <- SDL.setVideoMode scrWidth scrHeight scrBpp [SDL.SWSurface]
    SDL.setCaption caption []
    SDL.flip surface
    return $ View surface scr