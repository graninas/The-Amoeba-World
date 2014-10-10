module Amoeba.Middleware.OpenGL.Color where

import Amoeba.Middleware.OpenGL.Common

white, black, red, green, blue :: GLfColor4
white = color4 1 1 1 1
black = color4 0 0 0 1
red   = color4 1 0 0 1
green = color4 0 1 0 1
blue  = color4 0 0 1 1