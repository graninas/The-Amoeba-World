module Amoeba.View.Output.Render where

import Amoeba.View.Output.Color
import Amoeba.View.Output.Runtime
import Amoeba.GameLogic.Facade

import qualified Amoeba.Middleware.OpenGL.Facade as OGL

import qualified Data.Map as M

-- Temp function, for visual debug only.
-- TODO: do it right.
{-
getColorByPlayer pl | pl == humanPlayer = toSdlPixel white
getColorByPlayer pl | pl == ai1Player   = toSdlPixel red
getColorByPlayer pl = toSdlPixel blue


clearScreen surf = do
    p <- SDL.mapRGB (SDL.surfaceGetPixelFormat surf) 0 100 0
    SDL.fillRect surf Nothing p

renderCell surf plane (p, Object _ _ pl _ _ _) = do
    let col = getColorByPlayer pl
    let sdlRect = toSdlRect plane p
    Err.withLogError (SDL.rectangle surf sdlRect col) "renderCell: box failed."
    
renderWorldMap surf plane wm = mapM_ (renderCell surf plane) (M.toList wm)
renderTasksMap surf plane tm = return ()

renderBorders surf = do
    let rect = SDL.Rect 1 1 638 478
    SDL.rectangle surf rect (toSdlPixel white)

renderGame (View surf _ _ vPlane mbShift) game = renderGame' (shiftPlane mbShift)
  where
    renderGame' plane = do
        _ <- clearScreen surf
        _ <- renderBorders surf
        renderWorldMap surf plane (getWorldMap game)
        SDL.flip surf
    shiftPlane Nothing = vPlane
    shiftPlane (Just (p1, p2)) = vPlane +! p2 -! p1
-}

