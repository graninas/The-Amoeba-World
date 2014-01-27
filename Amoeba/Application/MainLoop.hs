module Application.MainLoop where

import Control.Wire
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Prelude hiding ((.), id)

import Middleware.Wire
import Application.Environment
import Application.Constants

-- | Evals main loop. Takes a wire to loop and start world.
startMainLoop wire world = do
    setupScreen screenSettings applicationName
    let gf = GameFlow 1 []
    let w = gameLoop wire clockSession
    writeFile logFile "Wire executing log:\n"
    execStateT (w gf) world

gameLoop w session gf = do
    (mx, w', session') <- stepSession w session gf
    case mx of
        Left ex -> return ()
        Right gf' -> gameLoop w' session' gf'

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
runWorld = updateWorld . periodically 1

runRender :: WWire GameFlow GameFlow
runRender = renderScene . periodically 0.1 -- TODO: Adjust FPS

idle :: WWire GameFlow GameFlow
idle = mkFix $ const Right

stepWorldState :: WStateIO W.Annotations
stepWorldState = do
    (w, anns) <- liftM W.stepWorld get
    put w
    return anns

updateWorld :: WWire GameFlow GameFlow
updateWorld = mkFixM $ \dt gf -> do
    anns <- stepWorldState
    let newGf = addAnnotationsEvent gf dt anns
    return $ Right newGf

renderScene :: WWire GameFlow GameFlow
renderScene = mkFixM $ \dt gf -> do
    w <- get
    liftIO $ renderSceneGraph scene w
    return . Right $ gf

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