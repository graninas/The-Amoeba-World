module Amoeba.Application.Game.GameDataLoader where

import Amoeba.GameLogic.Facade as GL

import Amoeba.Application.Assets.TestWorld

import qualified Amoeba.Middleware.Tracing.Log as Log
import Amoeba.Middleware.Tracing.ErrorHandling
import Paths_The_Amoeba_World as P


loadGame :: String -> IO Game
loadGame _ = return $ mkGame testWorld

loadGame' :: String -> IO Game
loadGame' worldPath = do
    worldFileRealName <- P.getDataFileName worldPath
    worldContents <- readFile worldFileRealName
    withLogError (return . not . null $ worldContents) $ "No data in data file " ++ worldPath
    case GL.parseWorld worldContents of
        Left err -> do
            Log.info $ "Error of loading world: " ++ err
            fail $ "Error of loading world: " ++ err
        Right w -> do
            Log.info $ "World loaded. Items in map: " ++ show (worldMapSize w)
            return $ mkGame w
