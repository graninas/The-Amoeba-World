module Amoeba.Middleware.Config.Extra where

import Amoeba.Middleware.Config.Config
import Control.Monad (liftM)

filePathLoader path file = liftM (++ file) $ strOption path