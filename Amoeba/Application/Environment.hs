module Application.Environment where

import Graphics.UI.SDL
import qualified Graphics.UI.SDL.TTF.General as TTF

import Control.Exception (bracket_, bracket)


withTtf = bracket_ TTF.init TTF.quit

withEnvironment action = withTtf (withInit [InitEverything] action)

setupScreen :: (Int, Int, Int) -> String -> IO ()
setupScreen (scrWidth, scrHeight, scrBpp) caption = do
    videoSurface <- setVideoMode scrWidth scrHeight scrBpp [SWSurface]
    setCaption caption []
    
    Graphics.UI.SDL.flip videoSurface