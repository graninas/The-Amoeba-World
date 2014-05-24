module View.Config where

import View.Runtime
import Middleware.Config.Facade

screenInfo = do
    sw <- intOption screenWidth
    sh <- intOption screenHeight
    cd <- intOption colorDepth
    return $ Screen sw sh cd

captionInfo = strOption appName

virtualPlainInfo = do
    virtX <- intOption virtualPlaneX
    virtY <- intOption virtualPlaneY
    return (virtX, virtY)

loadViewSettings cfg = do
    screen       <- extract cfg screenInfo
    caption      <- extract cfg captionInfo
    virtualPlain <- extract cfg virtualPlainInfo
    return (screen, caption, virtualPlain)