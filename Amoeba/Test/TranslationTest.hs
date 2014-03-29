module Main where

import GameLogic.Language.RawToken
import GameLogic.Language.Translation.Translator
import GameLogic.Language.Translation.Runtime
import GameLogic.Data.World

import Control.Monad (liftM)
import Control.Monad.State (runState)
import qualified Data.Map as M
import qualified Data.Monoid as Monoid

extractItemMap = liftM (trtObjectTemplateMap . snd)
extractLog = liftM (trtLog . snd)
extractResult = liftM fst
extractWorld = liftM (trtWorld . snd)

tryRight :: Monoid.Monoid m => Either n m -> m
tryRight (Right r) = r
tryRight _ = Monoid.mempty

main = do
    
    tokens <- readFile "./Data/Raws/World3.arf"
    let res = toWorld' runState tokens
    putStrLn $ unlines . tryRight $ extractLog res
    print $ tryRight $ extractItemMap res
    print $ extractResult res
    print $ extractWorld res

    --writeFile "./Data/Raws/World3.adt" (show . extractWorld $ res)
    
    putStrLn "Ok."