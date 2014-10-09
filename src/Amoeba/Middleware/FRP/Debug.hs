{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Amoeba.Middleware.FRP.Debug where

import Control.Wire
import Control.Wire.Unsafe.Event (Event(..))
import Control.Monad.IO.Class (liftIO, MonadIO(..))


printEventVal :: Show a => MonadIO m => forall s e. Wire s e m (Event a) (Event a)
printEventVal = mkGen_ $ \case
    ea@(Event a) -> (liftIO $ print a) >> (return $ Right ea)
    NoEvent -> (return $ Right NoEvent)