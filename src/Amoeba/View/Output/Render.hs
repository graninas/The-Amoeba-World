module Amoeba.View.Output.Render where

import Amoeba.View.Output.Runtime
import Amoeba.GameLogic.Facade

import Amoeba.Middleware.OpenGL.Facade as OGL
import Amoeba.Middleware.GLFW.Facade as GLFW

import qualified Data.Map as M

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
-}
--renderWorldMap surf plane wm = mapM_ (renderCell surf plane) (M.toList wm)
--renderTasksMap surf plane tm = return ()

renderBorders (w, h) = do
    OGL.color red
    renderPrimitive Lines $ do
        vertex $ Vertex2 (0 :: GLfloat) (realToFrac ( h) :: GLfloat)
        vertex $ Vertex2 (0 :: GLfloat) (realToFrac (-h) :: GLfloat)
    OGL.color white
    let ps = [0,2..] :: [GLfloat]
    let vFunc p = do
          vertex $ Vertex2 p 0
          vertex $ Vertex2 p (realToFrac h :: GLfloat)
          vertex $ Vertex2 0 p
          vertex $ Vertex2 (realToFrac w :: GLfloat) p
    let fFuncs = take 20 $ map vFunc ps
    mapM_ (renderPrimitive Lines) fFuncs


renderGame rt@(Runtime window uvp mbShift) game = do
    wSize <- GLFW.getWindowSize window
--    clear [ColorBuffer]
    renderBorders wSize
    --renderWorldMap surf plane (getWorldMap game)
    GLFW.swapBuffers window
--    shiftPlane Nothing = vPlane
--    shiftPlane (Just (p1, p2)) = vPlane +! p2 -! p1


