module Middleware.Config.Extra where

import Middleware.Config.Config
import Control.Monad (liftM)

filePathLoader path file = liftM (++ file) $ strOption path