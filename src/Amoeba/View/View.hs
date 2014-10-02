module Amoeba.View.View where

import Amoeba.View.Language
import Amoeba.View.Output.Types
import Amoeba.View.Output.Runtime
import Amoeba.View.Output.Utils


modifyView _ (StartViewPointMoving scrPt) view = view {runtimeVirtualPlainShift = Just (toUserViewPoint scrPt, toUserViewPoint scrPt)}
modifyView _ (ViewPointMoving scrPt) view@(Runtime _ _ mbShift) = let
    point' = toUserViewPoint scrPt
    in maybe view (\(shiftStart,_) -> view {runtimeVirtualPlainShift = Just (shiftStart, point')}) mbShift 
modifyView _ (StopViewPointMoving scrPt) view@(Runtime a vPlane mbShift) = let
    point' = toUserViewPoint scrPt
    in maybe view (\(p1, p2) -> (Runtime a (vPlane +! p2 -! p1) Nothing)) mbShift
    
-- TODO!
modifyView _ viewCommand _ = error $ "modifyView: view command missing: " ++ show viewCommand