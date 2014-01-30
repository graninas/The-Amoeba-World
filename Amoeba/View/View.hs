module View.View where

import View.Language

import qualified View.SdlFacade as SDL

data View = View { viewSurface :: SDL.Surface
                 , viewScreen :: Screen }


setupScreen :: Screen -> String -> IO View
setupScreen scr@(Screen w h bpp) caption = do
    surface <- SDL.setVideoMode w h bpp [SDL.SWSurface]
    SDL.setCaption caption []
    SDL.flip surface
    return $ View surface scr