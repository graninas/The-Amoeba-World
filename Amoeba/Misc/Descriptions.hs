module Misc.Descriptions where

import Control.Lens
import Data.Maybe (fromJust)

import GameLogic.Object
import GameLogic.Objects



describeNoProperty obj prop = "No property " ++ showedProp ++ " in object " ++ showedObj
  where
    showedProp = show . fromJust $ dummyObject ^? prop
    showedObj = show obj