module Application.Storage.CellularNetLoader where

import CellularNet

import Application.Game.Engine.Runtime
import qualified Middleware.Tracing.Log as Log
import Middleware.Tracing.ErrorHandling

{-
getSeed = return 100

loadGame worldPath = do
    worldContents <- readFile worldPath
    withLogError (return . not . null $ worldContents) $ "No data in data file " ++ worldPath
    case toWorld worldContents of
        Left err -> do
            Log.info $ "Error of loading world: " ++ err
            fail $ "Error of loading world: " ++ err
        Right w -> do
            Log.info $ "World loaded. Items in map: " ++ show (worldMapSize w)
            seed <- getSeed
            return $ mkGame w seed
            
-}