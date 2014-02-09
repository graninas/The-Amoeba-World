module Application.Storage.WorldLoader where

import GameLogic.Language.Translator

worldFile dataPath = dataPath ++ "Raws/world.arf"



loadWorld dataPath = do
    worldContents <- readFile dataPath
    return ()
    