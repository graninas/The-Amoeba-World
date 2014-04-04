{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Wire
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Prelude hiding ((.), id)

--import qualified Graphics.UI.SDL as SDL
--import qualified Graphics.UI.SDL.Primitives as SDL

--newtype GameStateTIO a = GameStateTIO (StateT () IO a)
--  deriving (Functor, Monad, MonadIO)
-- wrapGameState = GameStateTIO

type GameStateTIO = StateT () IO
type MyWire a b = Wire (Timed NominalDiffTime ()) () GameStateTIO a b

--startLoop :: MyWire () Int -> GameStateTIO Int
startLoop wire = do
    (delta, session') <- stepSession clockSession_
    (eitherResult, w') <- stepWire wire delta (Right ())
    liftIO $ putStrLn "In start loop!"
    loop' eitherResult w'

loop' (Left _) _ = return 0
loop' (Right x) w = do
    liftIO $ putStrLn "In loop!"
    return x

wire1 :: (HasTime t s) => Wire s () m a t
wire1 = time

wire2 :: MyWire () Int
wire2 = for 4 . pure (1 :: Int)

wire3 = mkGen_ $ \x -> do
    liftIO $ putStrLn "wire3"
    return $ Right 10


main :: IO ()
main = do
    
    res1 <- runStateT (startLoop wire1) ()
    print res1

    res2 <- runStateT (startLoop wire3) ()
    print res2
    
    testWire clockSession_ (for 1 . wire1)
    putStrLn "All Ok."