module Main where

import GameLogic.Language.RawToken
import GameLogic.Language.Translation.Translator
import GameLogic.Language.Translation.Runtime
import GameLogic.Data.World

import Control.Monad (liftM)
import Data.Either.Combinators
import qualified Data.Map as M

extractItemMap = liftM (trtObjectTemplateMap . snd)
extractLog = liftM (trtLog . snd)
extractResult = liftM fst

main = do
    
    tokens <- readFile "./Data/Raws/World3.arf"
    let res = toWorld tokens
    print $ extractLog res
    print $ extractItemMap res
    print $ extractResult res


    
    putStrLn "Ok."