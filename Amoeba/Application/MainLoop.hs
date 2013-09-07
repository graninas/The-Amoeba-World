module Application.MainLoop where

import Control.Wire
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Prelude hiding ((.), id)

import Middleware.Wire
import GameView.Render
import Application.Constants
import Application.GameFlow                 
import qualified World.World as W

-- | Evals main loop. Takes a wire to loop and start world.
startMainLoop wire world = do
    let gf = GameFlow 1 []
    let w = game wire clockSession
    writeFile logFile "Wire executing log:\n"
    execStateT (w gf) world

game w session gf = do
    (mx, w', session') <- stepSession w session gf
    case mx of
        Left ex -> return ()
        Right gf' -> game w' session' gf'

mainWire :: WWire GameFlow GameFlow
mainWire = for 15 . (   runMove
                    <|> runWorld
                    <|> runRender
                    <|> idle
                    )

reportMove :: WWire GameFlow GameFlow
reportMove = mkFixM $ \dt gf -> do
    liftIO $ putStrLn $ "Move: " ++ show (gameFlowMove gf)
    liftIO $ putStrLn $ "Time: " ++ show dt
    return . Right $ gf

nextMove :: WWire GameFlow GameFlow
nextMove = mkFixM $ \dt gf@(GameFlow m evs) -> do
    liftIO $ printGameFlow gf
    let newGf = GameFlow (succ m) [(dt, m, "playerMove")]
    return . Right $ newGf

runMove :: WWire GameFlow GameFlow
runMove = (nextMove . periodicallyI 10) <|> (reportMove . periodically 1)

runWorld :: WWire GameFlow GameFlow
runWorld = worldUpdate . periodically 1

runRender :: WWire GameFlow GameFlow
runRender = renderScene . periodically 0.1 -- TODO: Adjust FPS

idle :: WWire GameFlow GameFlow
idle = mkFix $ const Right

stepWorldState :: WStateIO W.Annotations
stepWorldState = do
    (w, anns) <- liftM W.stepWorld get
    put w
    return anns

worldUpdate :: WWire GameFlow GameFlow
worldUpdate = mkFixM $ \dt gf -> do
    anns <- stepWorldState
    let newGf = addAnnotationsEvent gf dt anns
    return $ Right newGf

renderScene :: WWire GameFlow GameFlow
renderScene = mkFixM $ \dt gf -> do
--    surface <- liftIO getVideoSurface
    return . Right $ gf


{-
render :: SDL.Surface -> SDLTTF.Font -> Frame -> IO ()
render screen font Frame{..} = do
  void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
    SDL.fillRect screen Nothing

  mapM_ renderAsteroid fAsteroids
  mapM_ (renderBounds . bounds) fBullets
  mapM_ renderPoint fParticles
  mapM_ renderUfo fUfo
  renderShip fShip

  scoreS <-
    SDLTTF.renderTextSolid font ("SCORE: " ++ show fScore)
      (SDL.Color 255 255 255)

  SDL.blitSurface scoreS Nothing screen (Just $ SDL.Rect 20 20 100 50)

  SDL.flip screen
-}

addThisMoveEvent gf@(GameFlow m evs) dt msg = GameFlow m ((dt, m, msg) : evs)
addAnnotationsEvent gf@(GameFlow m evs) dt anns = let
    preEvent = (dt, m, "World stepped. Annotations:")
    annsEvent = map (\a -> (dt, m, W.annotationMessage a)) anns
    in GameFlow m (preEvent : annsEvent ++ evs)

printGameFlow :: GameFlow -> IO ()
printGameFlow (GameFlow m evs) = do
    let moveStr = "[" ++ show m ++ "]"
    let evsStr = unlines . map show $ evs
    let resStr = moveStr ++ '\n' : evsStr
    appendFile logFile resStr