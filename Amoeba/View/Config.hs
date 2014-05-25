module View.Config where

import View.Runtime
import Application.Assets.ConfigScheme
import Middleware.Config.Facade

screenInfo = do
    sw <- intOption screenWidth
    sh <- intOption screenHeight
    cd <- intOption colorDepth
    return $ Screen sw sh cd

captionInfo = strOption appName

loadViewSettings cfg = do
    screen <- extract cfg screenInfo
    caption <- extract cfg captionInfo
    return (screen, caption)