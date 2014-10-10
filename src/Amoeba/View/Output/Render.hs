module Amoeba.View.Output.Render where

import Amoeba.View.Output.Runtime
import Amoeba.GameLogic.Facade

import Amoeba.Middleware.OpenGL.Facade as OGL
import Amoeba.Middleware.GLFW.Facade as GLFW

import qualified Data.Map as M

{-
getColorByPlayer pl | pl == humanPlayer = white
getColorByPlayer pl | pl == ai1Player   = red
getColorByPlayer pl                     = blue


clearScreen surf = do
    p <- SDL.mapRGB (SDL.surfaceGetPixelFormat surf) 0 100 0
    SDL.fillRect surf Nothing p

renderCell (p, Object _ _ pl _ _ _) = do
    let col = getColorByPlayer pl
    let sdlRect = toSdlRect plane p
    Err.withLogError (SDL.rectangle surf sdlRect col) "renderCell: box failed."

renderWorldMap wm = mapM_ renderCell (M.toList wm)
renderTasksMap tm = return ()
-}

renderBorders s = do
    color red
    rect (toVertex2 (0, 0)) (toVertex2 s)

renderTestLines (w, h) = do
    color blue
    let ps = [0,2..] :: [GLfloat]
    let vFunc p = do
          vertex $ Vertex2 p 0
          vertex $ Vertex2 p (realToFrac h :: GLfloat)
          vertex $ Vertex2 0 p
          vertex $ Vertex2 (realToFrac w :: GLfloat) p
    let fFuncs = take 20 $ map vFunc ps
    mapM_ (renderPrimitive Lines) fFuncs

shiftScene s = do
    matrixMode $= Modelview 0
    translate $ toVector3 s

renderGame rt@(Runtime window uvp mbShift) game = do
    wSize <- GLFW.getWindowSize window
    clear [ColorBuffer]
    shiftScene uvp
    renderTestLines wSize
    renderBorders wSize
    --renderWorldMap (getWorldMap game)
    GLFW.swapBuffers window
--    shiftPlane Nothing = vPlane
--    shiftPlane (Just (p1, p2)) = vPlane +! p2 -! p1


