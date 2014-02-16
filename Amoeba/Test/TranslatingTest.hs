module Main where

import GameLogic.Language.RawToken
import GameLogic.Language.Translating.Translator
import GameLogic.Runtime.World

import Control.Monad (liftM)


main = do
    
    tokens <- readFile "./Data/Raws/World3.arf"
    
    let res = toWorld tokens
    
    print res
    
    putStrLn "Ok."