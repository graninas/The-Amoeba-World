module View.Config where

import View.Language

import Middleware.Config.Facade
import Control.Monad (liftM3)

screenInfo = do
    sw <- intOption screenWidth
    sh <- intOption screenHeight
    cd <- intOption colorDepth
    return $ Screen sw sh cd

captionInfo = strOption appName

i = intOption
viewConfig' = liftM3 Screen (i screenWidth) (i screenHeight) (i colorDepth)
