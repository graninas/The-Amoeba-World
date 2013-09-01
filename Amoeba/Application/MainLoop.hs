{-# LANGUAGE Arrows #-}
module Application.MainLoop where

import Control.Wire
import Control.Arrow
import Control.Monad.State
import Prelude hiding ((.), id)

import Application.Constants                      
import qualified World.World as W

data GameFlow = GameFlow { gameFlowMove :: Int
                         , gameFlowEvents :: [(Time, Int, String)]
                         }
    deriving (Show)

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

type WStateIO = StateT W.World IO
type WWire a b = Wire () WStateIO a b

mainWire :: WWire GameFlow GameFlow
mainWire = for 15 . (   move
                    <|> runWorld
                    <|> idle
                    )

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

move :: WWire GameFlow GameFlow
move = (nextMove . periodicallyI 10) <|> (reportMove . periodically 1)

runWorld :: WWire GameFlow GameFlow
runWorld = worldUpdate . periodically 1

idle :: WWire GameFlow GameFlow
idle = mkFix $ const Right

stepWorldState = do
    (w, anns) <- get >>= return . W.stepWorld
    put w
    return anns

worldUpdate :: WWire GameFlow GameFlow
worldUpdate = mkFixM $ \dt gf -> do
    anns <- stepWorldState
    let newGf = addAnnotationsEvent gf dt anns
    return $ Right newGf


