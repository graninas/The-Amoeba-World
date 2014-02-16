module Application.Storage.WorldLoader where

import GameLogic.Language.Translator

worldFile dataPath = dataPath ++ "Raws/world.arf"



loadWorld dataPath = do
    worldContents <- readFile dataPath
    case toWorld worldContents of
        Left err -> putStrLn err
        Right w -> putStrLn "World loaded." >> return w
    
    
    
players = M.fromList [ ("Player0", D.dummyPlayer)
                     , ("Player1", D.player1) ]

translateWorldProperties _ [] = return w
translateWorldProperties w (IntProperty "width" i : ps) =
    translateWorldProperties (w {_width = i}) ps
translateWorldProperties w (IntProperty "height" i : ps) =
    translateWorldProperties (w {_height = i}) ps
translateWorldProperties w (ObjectProperty "defaultCell" obj : ps) = 

translateToWorld _ [] = Left "No data passed."
translateToWorld w (RP.EmptyToken : ts) = translateToWorld w ts
translateToWorld w (RP.World n prps : ts) = do
    w' <- translateWorldProperties w prps
    return $ translateToWorld w' ts