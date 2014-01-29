module GameView.Color where

import qualified Graphics.UI.SDL.Rect as SDL
import qualified Graphics.UI.SDL.Color as SDL
import Data.Word (Word8)
import Data.Bits

-- From here: https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8 .|.
                            255)
  where fi = fromIntegral

col1 = rgbColor 120 10 40
col2 = rgbColor 120 50 40
col3 = rgbColor 0 200 100
col4 = rgbColor 128 128 250
col5 = rgbColor 15 17 200
col6 = rgbColor 100 200 100
