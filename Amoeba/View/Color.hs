module View.Color where

import qualified Graphics.UI.SDL.Color as SDL
import Data.Word (Word8)
import Data.Bits (shiftL, (.|.))

-- From here: https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8 .|.
                            255)
  where fi = fromIntegral

toSdlPixel (r, g, b) = rgbColor r g b

black, white, red, green, blue :: (Word8, Word8, Word8)
black = (0, 0, 0)
white = (255, 255, 255)
red   = (255, 0, 0)
green = (0, 255, 0)
blue  = (0, 0, 255)