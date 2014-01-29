module View.View where

import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Types as SDL


data View = View { viewSurface :: SDL.Surface
                 , viewScreen :: Screen }
                 
getView :: IO View
getView = do
    surface <- SDL.getVideoSurface
    vInfo <- SDL.getVideoInfo
    let w = SDL.videoInfoWidth vInfo
    let h = SDL.videoInfoHeight vInfo
    return $ View surface (Screen w h)


setupScreen :: (Screen, Int) -> String -> IO View
setupScreen (scr@(Screen scrWidth scrHeight), scrBpp) caption = do
    surface <- SDL.setVideoMode scrWidth scrHeight scrBpp [SDL.SWSurface]
    SDL.setCaption caption []
    SDL.flip surface
    return $ View surface scr