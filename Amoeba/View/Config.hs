module View.Config where

import Middleware.Config.Facade

data Screen = Screen { width :: Int
                     , height :: Int
                     }
    deriving (Show, Read, Eq)


viewConfig = do
    sw <- intOption screenWidth
    sh <- intOption screenHeight
    return $ Screen sw sh


