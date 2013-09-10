module Test.PropertiesTest where


import World.Properties
import World.Geometry

import Test.Data
import Test.Arbitraries

prop_moving p dir = let property = moving dir . dislocation p
                        newPoint = movePoint p dir
                    in (query dislocation (tick property) == newPoint) && (p /= newPoint)

prop_notMoving p dir = let property = dislocation p
                       in query dislocation (tick property) == p