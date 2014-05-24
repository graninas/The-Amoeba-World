module View.View where

import View.Color
import View.Runtime
import GameLogic.Facade

import qualified Middleware.SDL.SDLFacade as SDL
import qualified Middleware.SDL.Render as SDL
import qualified Middleware.Tracing.Log as Log
import Middleware.Tracing.ErrorHandling

import qualified Data.Map as M


-- TODO
scale = 10
cellSide = 7

setupView :: (Screen, String, ViewPoint) -> IO View
setupView (scr@(Screen w h bpp), caption, virtualPlane) = do
    surface <- SDL.setVideoMode w h bpp [SDL.SWSurface]
    SDL.setCaption caption []
    SDL.flip surface
    return $ View surface scr caption virtualPlane Nothing
    
--------------------------------------------------------------------
-- Temp functions, for visual debug only.
-- TODO: do it right.

getColorByPlayer pl | pl == dummyPlayer = toSdlPixel white
getColorByPlayer pl | pl == player1     = toSdlPixel red
getColorByPlayer pl | pl == player2     = toSdlPixel blue

toSdlRect :: Point -> SDL.Rect
toSdlRect p = SDL.Rect x y (x + cellSide) (y + cellSide)
  where
    x = pointX p * scale
    y = pointY p * scale
    
renderCell surf (p, Object _ _ pl _ _ _) = do
    let col = getColorByPlayer pl
    let sdlRect = toSdlRect p
    withLogError (SDL.rectangle surf sdlRect col) "renderCell: box failed."
    return ()

renderWorldMap surf w h wm = mapM_ (renderCell surf) (M.toList wm)

renderBorders surf = do
    let rect = SDL.Rect 1 1 638 478
    SDL.rectangle surf rect (toSdlPixel white)

renderWorld surf (World wm _ w h _) = do
    renderBorders surf
    renderWorldMap surf w h wm
