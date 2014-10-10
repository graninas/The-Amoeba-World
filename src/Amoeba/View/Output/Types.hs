module Amoeba.View.Output.Types where

import Data.Word (Word16)

type ViewPointCoordinates = (Int, Int)
type UserViewPoint = ViewPointCoordinates
type ScreenPoint = (Word16, Word16)

data Screen = Screen Int Int Int


