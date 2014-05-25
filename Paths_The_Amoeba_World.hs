module Paths_The_Amoeba_World where

import System.FilePath

gameDataPath = "./Game/Data/"

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (</>) gameDataPath