module View.Config where

import Middleware.Config.Facade
import Control.Monad

data Screen = Screen { width :: Int
                     , height :: Int
                     , depth :: Int
                     }
    deriving (Show, Read, Eq)


viewConfig = do
    sw <- intOption screenWidth
    sh <- intOption screenHeight
    cd <- intOption colorDepth
    return $ Screen sw sh cd

i = intOption
viewConfig' = liftM3 Screen (i screenWidth) (i screenHeight) (i colorDepth)
