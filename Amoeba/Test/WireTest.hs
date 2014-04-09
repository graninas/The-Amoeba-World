module Main where

import Control.Wire
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Prelude hiding ((.), id)
import Data.Monoid

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Events as SDL

data InhibitAction = QuitAction String
                   | ContinueAction
  deriving (Show, Eq)

instance Monoid InhibitAction where
    mempty = QuitAction "Inhibited."
    (QuitAction s1) `mappend` (QuitAction s2) = QuitAction (s1 ++ s2)
    ContinueAction `mappend` ContinueAction = ContinueAction
    (QuitAction s) `mappend` _ = (QuitAction s)
    _ `mappend` (QuitAction s) = (QuitAction s)

data WireOutput = Quit String
  deriving (Show, Eq)

type GameStateTIO = StateT () IO
type MyWire a b = Wire (Timed NominalDiffTime ()) InhibitAction GameStateTIO a b

startLoop = loop' clockSession_ (Right 1)

loop' _ (Left (QuitAction res)) _ = return (Quit res)
loop' s (Right _) w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta (Right 1)
    liftIO $ print delta
    loop' s' eitherResult w'


pollEventWire :: MyWire () SDL.Event
pollEventWire = mkGen_ $ \_ -> do
    e <- liftIO SDL.pollEvent
    liftIO $ print e
    return $ Right e

eventMapperWire :: MyWire SDL.Event ()
eventMapperWire = mkPure_ $ \event -> case event of
    SDL.NoEvent -> Right ()
    SDL.Quit -> Left (QuitAction "Finished.")
    e -> Left . QuitAction ("Event not supported: " ++ show e)

solverWire = eventMapperWire . pollEventWire
pureWire = for 3 . pure 10

main :: IO ()
main = do
    
    {-
    res1 <- runStateT (startLoop wire1) ()
    print res1

    res2 <- runStateT (startLoop wire2) ()
    print res2
    -}
    
--    res3 <- runStateT (startLoop wire3) ()
--    print res3

    e <- runStateT (startLoop pureWire) ()
    print e

--    testWire clockSession_ (for 1 . wire1)
    putStrLn "All Ok."