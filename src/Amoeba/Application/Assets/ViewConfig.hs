module Amoeba.Application.Assets.ViewConfig where

import Amoeba.Application.Assets.ConfigScheme
import Amoeba.Middleware.Config.Facade
import Amoeba.View.Facade as V

screenInfo = do
    sw <- intOption screenWidth
    sh <- intOption screenHeight
    cd <- intOption colorDepth
    return $ V.Screen sw sh cd

captionInfo = strOption appName

virtualPlainInfo = do
    virtX <- intOption virtualPlaneX
    virtY <- intOption virtualPlaneY
    return (virtX, virtY)

loadViewSettings cfg = do
    screen <- extract cfg screenInfo
    caption <- extract cfg captionInfo
    virtualPlain <- extract cfg virtualPlainInfo
    return (screen, caption, virtualPlain)