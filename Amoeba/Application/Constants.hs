module Application.Constants where

import Graphics.UI.SDL.Color

logFile = "log.txt"

screenSettings :: (Int, Int, Int)
screenSettings = (640, 480, 32)
defaultFont = "lazy.ttf"

activeMenuColor = Color 255 255 255
inactiveMenuColor = Color 127 127 127
menuItemDY = textSize + 2


textSize = 28 :: Int