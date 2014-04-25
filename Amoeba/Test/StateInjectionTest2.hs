module Main where

import Control.Monad.State
import Control.Monad
import System.Random

newtype Context = Context { ctxNextId :: State Context Int
                          , ctxNextName :: State Context String }

getNextId = get >>= ctxNextId
getNextName = get >>= ctxNextName


-- Client code. Knows nothing about furniture materials, but uses external state to create it.
type IkeaFurniture = (Int, Name)

createFurniture :: State Context IkeaFurniture
createFurniture = litfM2 (,) getNextId getNextName

ikea :: State Context [IkeaFurniture]
ikea = do
    table <- createFurniture
    shelf <- createFurniture
    return [table, shelf]





-- The state, which will be injected into client code.
nextId :: Int -> State Context Int
nextId prevId = do let nId = prevId + 1
                   modify (\ctx -> ctx { ctxNextId = nextId nId })
                   return nId

nextRnd :: StdGen -> State Context Int
nextRnd prevG = do let (r, g) = random prevG
                   modify (\ctx -> ctx { ctxNextId = nextRnd g })
                   return r

nextName :: Int -> State Context String
nextName 0 = do
    modify (\ctx -> ctx { ctxNextName = nextName 1 } )
    return "ГНВОЕРК"
nextName 1 = do
    modify (\ctx -> ctx { ctxNextName = nextName 2 } )
    return "РИКТИГ ЁГЛА"
nextName _ = return ""

main :: IO ()
main = do
    print "Just increment:"
    let nextIdF = nextId 0
    print $ evalState worker (Context nextIdF nextIdF)
    
    print "Random Id:"
    let g = mkStdGen 100
    let nextRndF = nextRnd g
    print $ evalState worker (Context nextRndF nextRndF)