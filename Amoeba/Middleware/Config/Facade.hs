module Middleware.Config.Facade (module X) where
import Data.ConfigFile as X
import Data.Either.Utils as X (forceEither)
import Middleware.Config.Config as X (getConfig, loadConfiguration)
import Middleware.Config.Scheme as X

{- Nice syntax shugar! The same:

module Middleware.Config.Facade (
      module Middleware.Config.Config
    , module Middleware.Config.Scheme
    , module Data.ConfigFile
    ) where

import Data.ConfigFile
import Middleware.Config.Config (getConfig)
import Middleware.Config.Scheme

-}