{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Middleware.Tracing.ErrorHandling where

import qualified Middleware.Tracing.Log as Log

import Control.Monad (unless)
import Control.Monad.IO.Class

class Checkable m a where
  check :: m a -> m Bool

instance Checkable IO Bool where
  check = id

withLogError :: forall m. MonadIO m => Checkable m act => m act -> String -> m ()
withLogError act msg = do
    res <- check act
    unless res $ liftIO $ Log.error msg >> error msg