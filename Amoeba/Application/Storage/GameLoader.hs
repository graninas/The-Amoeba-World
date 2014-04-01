module Application.Storage.GameLoader where

import GameLogic.Data.Game
import GameLogic.Language.Translation.Translator
import Application.Game.Runtime
import qualified Middleware.Tracing.Log as Log

-- TODO: get seed from timer
getSeed = return 100

loadGame worldPath = do
    worldContents <- readFile worldPath
    case toWorld worldContents of
        Left err -> do
            -- TODO: this function should be in Either monad.
            Log.info $ "Error of loading world: " ++ err
            fail $ "Error of loading world: " ++ err
        Right w -> do
            Log.info "World loaded."
            seed <- getSeed
            return $ mkGame w seed
