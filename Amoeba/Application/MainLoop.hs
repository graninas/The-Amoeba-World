module Application.MainLoop where

import qualified Application.Wire as W

import World.World


-- | Evals main loop. Takes a wire to loop and start world.
startMainLoop wire = loop wire W.clockSession
  where
    loop w session world = do
        (mx, w', session') <- W.stepSession w session world
        case mx of
          Left ex -> error "Inhibition is undefined."
          Right newWorld -> loop w' session' newWorld
          
mainLoopWire :: W.Wire () IO World World
mainLoopWire = undefined