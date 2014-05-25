module Paths_The_Amoeba_World where

import System.FilePath

-- This module used by cabal-install.

gameDataPath = "./Game/Data/"

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (</>) gameDataPath