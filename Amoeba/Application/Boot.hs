module Application.Boot where

import View.Config
import Middleware.Config.Facade

boot cfg = do
    
    viewCfg <- getConfig cfg viewConfig'
    
    
    print viewCfg