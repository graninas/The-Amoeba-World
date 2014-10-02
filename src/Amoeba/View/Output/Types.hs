module Amoeba.View.Output.Types where

import Data.Word (Word16)

type UserViewPoint = (Int, Int)
type ScreenPoint = (Word16, Word16)

data Screen = Screen Int Int Int


toUserViewPoint :: ScreenPoint -> UserViewPoint
toUserViewPoint (x, y) = (fromIntegral x, fromIntegral y)
