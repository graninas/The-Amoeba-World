module Main where

import GameLogic.Language.RawToken
import GameLogic.Language.Translation.Translator
import GameLogic.Language.Translation.Runtime
import GameLogic.Data.World

import Control.Monad (liftM)
import Data.Either.Combinators
import qualified Data.Map as M
import qualified Data.Monoid as Monoid

extractItemMap = liftM (trtObjectTemplateMap . snd)
extractLog = liftM (trtLog . snd)
extractResult = liftM fst

tryRight :: Monoid.Monoid m => Either n m -> m
tryRight (Right r) = r
tryRight _ = Monoid.mempty

main = do
    
    tokens <- readFile "./Data/Raws/World3.arf"
    let res = toWorld tokens
    putStrLn $ unlines . tryRight $ extractLog res
    print $ tryRight $ extractItemMap res
    print $ extractResult res


    
    putStrLn "Ok."