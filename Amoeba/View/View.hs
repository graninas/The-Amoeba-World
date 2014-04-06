module View.View where

import View.Language
import View.Color

import qualified Middleware.SDL.SDLFacade as SDL

type ViewSurface = SDL.Surface

data View = View { viewSurface :: ViewSurface
                 , viewScreen :: Screen
                 , viewCaption :: String }

setupView :: (Screen, String) -> IO View
setupView (scr@(Screen w h bpp), caption) = do
    surface <- SDL.setVideoMode w h bpp [SDL.SWSurface]
    SDL.setCaption caption []
    SDL.flip surface
    return $ View surface scr caption
    
    
clearScreen surf = SDL.fillRect surf Nothing black