module Application.Storage.WorldLoader where

worldFile dataPath = dataPath ++ "world.dat"



loadGame dataPath = do
    putStrLn $ worldFile dataPath
    