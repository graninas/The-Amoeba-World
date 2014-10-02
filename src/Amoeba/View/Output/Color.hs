module Amoeba.View.Output.Color where

import Data.Word (Word8)
import Data.Bits (shiftL, (.|.))

black, white, red, green, blue :: (Word8, Word8, Word8)
black = (0, 0, 0)
white = (255, 255, 255)
red   = (255, 0, 0)
green = (0, 255, 0)
blue  = (0, 0, 255)