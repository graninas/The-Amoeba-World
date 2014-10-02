module Amoeba.GameLogic.Runtime where

import Amoeba.GameLogic.Data.Facade as GL

import Control.Concurrent as C

-- N.B.: experimental mechanism.
-- TODO: use lens

data RunState = Initializing
              | Running [C.ThreadId]
              | Finishing [C.ThreadId]
              | Finished
  deriving (Show, Eq)
  
data GameRuntime = GameRuntime { grtRunState :: RunState
                               , grtGame :: Game
                               }

initialGameRuntime :: Game -> GameRuntime
initialGameRuntime = GameRuntime Initializing

isFinished :: GameRuntime -> Bool
isFinished (GameRuntime Finished _) = True
isFinished _ = False

isInitialising :: GameRuntime -> Bool
isInitialising (GameRuntime Initializing _) = True
isInitialising _ = False

--setRunFinished :: GameRuntime -> GameRuntime
--setRunFinished (GameRuntime tr g) = GameRuntime tr (GL.setGameFinished g)

--setRunRunning :: GameRuntime -> GameRuntime
--setRunRunning (GameRuntime tr g) = GameRuntime tr (GL.setGameRunning g)

startRunning :: [C.ThreadId] -> GameRuntime -> GameRuntime
startRunning tr (GameRuntime Initializing g) = GameRuntime (Running tr) g
-- TODO: maybe, do it with Either/Maybe?
startRunning _ (GameRuntime st _) = error $ "Can't start running - it is in invalid state: " ++ show st

beginFinishing :: GameRuntime -> GameRuntime
beginFinishing (GameRuntime (Running []) g) = GameRuntime Finished g
beginFinishing (GameRuntime (Running tr) g) = GameRuntime (Finishing tr) g
beginFinishing (GameRuntime st _) = error $ "Can't begin finishing - it is in invalid state: " ++ show st

getState :: GameRuntime -> RunState
getState (GameRuntime rs _) = rs