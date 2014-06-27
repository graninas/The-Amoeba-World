module View.View where

import View.Color
import View.Runtime
import GameLogic.Facade

import qualified Middleware.SDL.SDLFacade as SDL
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
    
clearScreen surf = do
    p <- SDL.mapRGB (SDL.surfaceGetPixelFormat surf) 0 100 0
    SDL.fillRect surf Nothing p

--------------------------------------------------------------------
-- Temp functions, for visual debug only.
-- TODO: do it right.

getColorByPlayer pl | pl == humanPlayer = toSdlPixel white
getColorByPlayer pl | pl == ai1Player   = toSdlPixel red
getColorByPlayer pl = toSdlPixel blue

toSdlRect :: ViewPoint -> Point -> SDL.Rect
toSdlRect (planeX, planeY) point = SDL.Rect x' y' (x' + cellSide) (y' + cellSide)
  where
    x = pointX point
    y = pointY point
    x' = x * scale + planeX
    y' = y * scale + planeY

renderCell surf plane (p, Object _ _ pl _ _ _) = do
    let col = getColorByPlayer pl
    let sdlRect = toSdlRect plane p
    withLogError (SDL.rectangle surf sdlRect col) "renderCell: box failed."
    return ()
    

renderWorldMap surf plane wm = mapM_ (renderCell surf plane) (M.toList wm)
renderTasksMap surf plane tm = return ()


renderBorders surf = do
    let rect = SDL.Rect 1 1 638 478
    SDL.rectangle surf rect (toSdlPixel white)

renderGame (View surf _ _ vPlane mbShift) (Game (World wm _ tm _ _) _) = renderGame' (shiftPlane mbShift)
  where
    renderGame' plane = do
        clearScreen surf
        renderBorders surf
        renderWorldMap surf plane wm
        renderTasksMap surf plane tm
        SDL.flip surf
    shiftPlane Nothing = vPlane
    shiftPlane (Just (p1, p2)) = vPlane +! p2 -! p1
    
