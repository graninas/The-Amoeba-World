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


black = rgbColor 0 0 0