module Amoeba.Middleware.OpenGL.Primitives where

import Amoeba.Middleware.OpenGL.Common

import Graphics.Rendering.OpenGL as OGL

{-
v1
_____
|   /|
|  / |
| /  |
|/___|
      v2

-}


quad :: GLfVertex2 -> GLfVertex2 -> IO ()
quad v1@(Vertex2 x1 y1) v2@(Vertex2 x2 y2) = renderPrimitive TriangleStrip $ do
    vertex   v1
    vertex $ vertex2 x2 y1
    vertex $ vertex2 x1 y2
    vertex   v2
    
