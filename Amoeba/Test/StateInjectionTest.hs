{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad.State
import Control.Monad
import System.Random

data Context = Context { ctxNextId :: State Context Int }





-- client code. Knowns nothing about random gens, but uses external state.
getNextId = do
    ctx <- get
    ctxNextId ctx

getNextId' = get >>= ctxNextId

worker :: State Context (Int, Int)
worker = do
    n1 <- getNextId
    n2 <- getNextId
    return (n1, n2)


-- The state, wich will be injected into client code.
nextId :: Int -> State Context Int
nextId prevId = let nId = prevId + 1
                in do
                   ctx <- get
                   put $ ctx { ctxNextId = nextId nId }
                   return nId

nextRnd :: StdGen -> State Context Int
nextRnd prevG = let (r, g) = random prevG
                in do
                   ctx <- get
                   put $ ctx { ctxNextId = nextRnd g }
                   return r



tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    print "Just increment:"
    print $ evalState worker (Context $ nextId 0)
    
    print "Random Id:"
    let g = mkStdGen 100
    print $ evalState worker (Context $ nextRnd g) 