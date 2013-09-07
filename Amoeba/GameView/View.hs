module GameView.View where

import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Types as SDL

data Screen = Screen { screenWidth :: Int
                     , screenHeight :: Int }

data View = View { viewSurface :: SDL.Surface
                 , viewScreen :: Screen }
                 
getView :: IO View
getView = do
    surface <- SDL.getVideoSurface
    vInfo <- SDL.getVideoInfo
    let w = SDL.videoInfoWidth vInfo
    let h = SDL.videoInfoHeight vInfo
    return $ View surface (Screen w h)