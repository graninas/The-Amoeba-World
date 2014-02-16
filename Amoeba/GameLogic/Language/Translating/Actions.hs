module GameLogic.Language.Translating.Actions where

import GameLogic.Language.RawToken
import GameLogic.Language.Translating.Runtime

import Prelude hiding (log)
import Control.Monad.Trans.Either (left)

(/>) trigger act = \token -> if trigger token
                             then act token
                             else logExt $ "Token not triggered: " ++ show token


skip, addItem :: RawToken -> Trans ()
skip t =  log $ "Skip for: " ++ show t

addItem (Item name props) = do
    log $ "Adding object template for: " ++ show name
    insertObjectTemplate name props

addItem t = left $ "addItem: Item expected but got " ++ show t


setupWorld (World name props) = do
    log $ "Setting World."
    
    
setupWorld t = left $ "setupWorld: World expected but got " ++ show t