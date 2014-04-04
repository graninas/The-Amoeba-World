module Main where

import Control.Wire
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Prelude hiding ((.), id)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Events as SDL

type GameStateTIO = StateT () IO
type MyWire a b = Wire (Timed NominalDiffTime ()) () GameStateTIO a b

data GameLoopAction = Quit String
                    | SdlEvent SDL.Event
                    | IntResult Int
                    | TimeResult NominalDiffTime

startLoop wire = do
    (delta, session') <- stepSession clockSession_
    (eitherResult, w') <- stepWire wire delta (Right ())
    liftIO $ putStrLn "In start loop!"
    loop' eitherResult w'

loop' (Left _) _ = error "Inhibited."
loop' (Right x) w = do
    liftIO $ putStrLn "In loop!"
    return x

wire1 :: (HasTime t s) => Wire s () m a t
wire1 = time

wire2 :: MyWire () Int
wire2 = for 4 . pure (1 :: Int)

wire3 :: MyWire () Int
wire3 = mkGen_ $ \_ -> do
    liftIO $ putStrLn "wire3"
    return $ Right 10

pollEventWire :: MyWire () SDL.Event
pollEventWire = mkGen_ $ \_ -> liftM Right (liftIO SDL.pollEvent)

eventMapperWire :: MyWire SDL.Event Int
eventMapperWire = mkPure_ $ \event -> case event of
    SDL.NoEvent -> Left ()
    _ -> Right 100000

solverWire = eventMapperWire . pollEventWire

main :: IO ()
main = do
    
    res1 <- runStateT (startLoop wire1) ()
    print res1

    res2 <- runStateT (startLoop wire2) ()
    print res2

    res3 <- runStateT (startLoop wire3) ()
    print res3

    e <- runStateT (startLoop solverWire) ()
    print e

    --testWire clockSession_ (for 1 . wire1)
    putStrLn "All Ok."