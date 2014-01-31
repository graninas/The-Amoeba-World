module Application.Runtime.Engine where

import Application.Runtime.Language

import Control.Wire
import Control.Monad.State
import Prelude hiding ((.), id)

import Middleware.Config.Facade
import View.View

startMainLoop :: Configuration -> View -> WWire Int Int -> IO Int
startMainLoop cfg view wire = do
    let rt = runtime cfg view
    (s, session') <- stepSession clockSession_
    let state = gameLoop wire s (Right 10)
    let newState = execStateT state rt
    return 10

gameLoop wire s input = do
    (output, w') <- stepWire wire s input
    case output of
        Left ex -> return 10
        Right x -> return 20


{-
startMainLoop cfg view wire = do
    let rt = runtime cfg view
    let w = gameLoop wire (Right ())
    execStateT w rt
    

gameLoop w input = do
    s <- clockSession_
    (output, w') <- stepWire w s input
    case output of
        Left ex -> return ex
        Right x -> gameLoop w' x
        
        
-}