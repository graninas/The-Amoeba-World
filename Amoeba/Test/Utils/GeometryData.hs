module Test.Utils.GeometryData where

import GameLogic.Base.Geometry

point1 = point 0 (-10) 0
point2 = point 3 (-3) 0
point3 = point (-10) 0 0
point4 = point 5 (-3) 0
point5 = point (-10) (-3) 0
point6 = point 5 0 0

rect1 = rectBound point2 point6
rect2 = rectBound point5 point6