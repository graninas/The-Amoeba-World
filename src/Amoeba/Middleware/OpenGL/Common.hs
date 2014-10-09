module Amoeba.Middleware.OpenGL.Common where

import Graphics.Rendering.OpenGL as OGL

type GLfVertex2   = Vertex2   GLfloat
type GLfVertex3   = Vertex3   GLfloat
type GLfVector2   = Vector2   GLfloat
type GLfVector3   = Vector3   GLfloat
type GLfColor3    = Color3    GLfloat
type GLfColor4    = Color4    GLfloat
type GLfTexCoord2 = TexCoord2 GLfloat


vertex3   :: GLfloat -> GLfloat -> GLfloat -> GLfVertex3
vector3   :: GLfloat -> GLfloat -> GLfloat -> GLfVector3
color3    :: GLfloat -> GLfloat -> GLfloat -> GLfColor3
color4    :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfColor4
texCoord2 :: GLfloat -> GLfloat -> GLfTexCoord2

vertex3   = Vertex3
vector3   = Vector3
color3    = Color3
color4    = Color4
texCoord2 = TexCoord2
