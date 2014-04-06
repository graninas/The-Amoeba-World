{-# LANGUAGE MultiParamTypeClasses #-}
module Middleware.Tracing.ErrorHandling where

import qualified Middleware.Tracing.Log as Log

import Control.Monad (unless)
import Control.Monad.IO.Class

class Checkable m a where
  check :: m a -> m Bool

instance Checkable IO Bool where
  check = id

--{-# LANGUAGE RankNTypes #-}
--withLogError :: forall m. MonadIO m => Checkable m act => m act -> String -> m ()
withLogErrorIO act msg = do
    res <- check act
    unless res $ liftIO $ Log.error msg >> error msg
    
withLogError act msg = liftIO $ withLogErrorIO act msg