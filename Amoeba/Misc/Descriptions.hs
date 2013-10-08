module Misc.Descriptions where

import Control.Lens
import Data.Maybe (fromJust)

import GameLogic.Object
import GameLogic.Objects

nameProperty prop = show . fromJust $ dummyObject ^? prop

describeNoProperty obj prop = "No property " ++ showedProp ++ " in object " ++ showedObj
  where
    showedProp = nameProperty prop
    showedObj = show obj
    
