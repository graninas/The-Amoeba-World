module Main where

import Control.Wire
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Prelude hiding ((.), id)

--import qualified Graphics.UI.SDL as SDL
--import qualified Graphics.UI.SDL.Primitives as SDL

wire1 :: (HasTime t s) => Wire s () m a t
wire1 = time


type GameStateTIO = StateT () IO
type MyWire a b = Wire (Timed NominalDiffTime ()) () GameStateTIO a b

newtype MyIoWire a = MyIoWire (MyWire () a)

instance Monad MyIoWire where
    return = MyIoWire . pure
    ma >>= f = do
        r <- ma
        f r

instance MonadIO MyIoWire where
    liftIO = undefined

startLoop :: MyWire () Int -> GameStateTIO Int
startLoop wire = do
    (delta, session') <- stepSession clockSession_
    (eitherResult, w') <- stepWire wire delta (Right ())
    liftIO $ putStrLn "In start loop!"
    loop' eitherResult w'

loop' (Left _) _ = return 0
loop' (Right x) w = do
    liftIO $ putStrLn "In loop!"
    return x

wire2 :: MyWire () Int
wire2 = for 4 . pure (1 :: Int)

wire3 :: MyIoWire ()
wire3 = do
    liftIO $ putStrLn "In wire3!"

main :: IO ()
main = do
    
    res <- runStateT (startLoop wire2) ()
    print res
    
    testWire clockSession_ (for 1 . wire1)
    putStrLn "All Ok."