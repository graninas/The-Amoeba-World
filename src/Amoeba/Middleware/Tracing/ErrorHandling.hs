{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Amoeba.Middleware.Tracing.ErrorHandling where

import qualified Amoeba.Middleware.Tracing.Log as Log

import Control.Monad (unless)
import Control.Monad.IO.Class

class Checkable m a where
  check :: m a -> m Bool

instance Checkable IO Bool where
  check = id

-- This function uses logger. It seems wrong: logger can be uninitialized.

withLogError :: forall m. MonadIO m => Checkable m act => m act -> String -> m ()
withLogError act msg = do
    res <- check act
    unless res $ liftIO $ Log.error msg >> error msg

catchWithLogError :: forall m. MonadIO m => Checkable m act => m act -> String -> m Bool
catchWithLogError act msg = do
    res <- check act
    unless res $ liftIO $ Log.error msg >> error msg
    return res
    
a ?>  b = withLogError a b
a ?-> b = catchWithLogError a b