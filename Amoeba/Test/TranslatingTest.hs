module Main where

import GameLogic.Language.RawToken
import GameLogic.Language.Translating.Translator
import GameLogic.Language.Translating.Runtime
import GameLogic.Runtime.World

import Control.Monad (liftM)
import Data.Either.Combinators

extractItemMap = liftM (trtItemMap . snd)
extractLog = liftM (trtLog . snd)
extractResult = liftM fst

main = do
    
    tokens <- readFile "./Data/Raws/World3.arf"
    let res = toWorld tokens
    putStrLn $ unlines . fromRight' . extractLog $ res
    putStrLn $ show . fromRight' . extractItemMap $ res
    putStrLn $ show . fromRight' . extractResult $ res

    


    
    putStrLn "Ok."