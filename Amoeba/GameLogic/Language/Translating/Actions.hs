module GameLogic.Language.Translating.Actions where

import GameLogic.Language.RawToken
import GameLogic.Language.Translating.Runtime

import Prelude hiding (log)


(/>) trigger act = \token -> if trigger token
                             then act token
                             else return $ "Token not triggered: " ++ show token


skip, addItem :: RawToken -> Trans String
skip t = do
    log $ "Skip for: " ++ show t
    return ""
    
addItem (Item name props) = do
    log $ "Adding object template for: " ++ show name
    insertObjectTemplate name props
    return ""
addItem t = return $ "addItem: Item expected but got " ++ show t