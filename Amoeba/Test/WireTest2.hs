module Test.WireTest2 where

import Application.Game.Engine.Types
import Application.Game.Engine.Core
import Application.Game.Runtime

import Middleware.FRP.NetwireFacade
import Prelude hiding (id, (.))

import Control.Monad.State
import System.Random

type TestWire a b = GWire GameStateTIO a b
type FakeSdlEvent = Int

diagnose m = mkGen_ $ \_ -> withIO $ putStrLn m

selector True = diagnose "selector True"
selector False = quit . diagnose "selector False"

-- | Modes wire:
-- when input from internal wire is ((), True), selects True and just prints 'selector True'.
-- when input from internal wire is ((), False), selects False, prints 'selector False' and inhibits with 'quit'.
-- Internal wire:
-- for the first 2 seconds acts like 'Left wire' returning ((), True). After 2 seconds the left wire inhibits and
-- switches to the 'Right wire' returning ((), False). It should hold for 3 second, but the Modes wire will inhibit now.
wire1 :: TestWire () ()
wire1 = modes True selector . 
    (
        -- Internal wire:
            (diagnose "left"  &&& for 2 . now . pure True)  -- Left wire,  returning ((), True)
        --> (diagnose "right" &&& for 3 . now . pure False) -- Right wire, returning ((), False)
    )
    

wire2 = for 1 . diagnose "1" 
        --> for 1 . processFakeSdlEvent . pollFakeSdlEvent
        --> for 2 . diagnose "2"
        --> wire2
        
pollFakeSdlEvent :: TestWire () FakeSdlEvent
pollFakeSdlEvent = mkGen_ $ \_ -> do
    liftIO $ putStrLn "Polling..."
    g <- liftIO newStdGen
    let r = randomR (1, 3) g
    return $ Right r

processFakeSdlEvent :: TestWire FakeSdlEvent ()
processFakeSdlEvent = mkPure_ $ \event -> case event of
    0 -> Left "Quit event: Finished."
    1 -> Right ()
    2 -> Right ()
    3 -> Right ()
    e -> Left $ "Event not supported: " ++ show e

render = diagnose "Rendering..."

main = do


    putStrLn "All Ok."
