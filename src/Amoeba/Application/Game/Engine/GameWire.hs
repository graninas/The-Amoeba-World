{-# LANGUAGE RankNTypes #-} 
module Amoeba.Application.Game.Engine.GameWire where

import Amoeba.Middleware.FRP.NetwireFacade
import Amoeba.Application.Game.Engine.Runtime

import Prelude hiding ((.), id)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

type Inhibitor = String
type GTime = Timed NominalDiffTime ()
type GWire state a b = Wire GTime Inhibitor state a b


-- TODO: HACK: FIXME: needs deep generalization of wires mechanism.
-- Will do it later. Now, this is a HACK and copy-paste.

type ViewWire a b        = GWire ViewTIO a b
type GameStorageWire a b = GWire GameStorageTIO a b
type AIPlayerWire a b    = GWire AIPlayerTIO a b


-- Combinator wires
quitWith = inhibit
quit = inhibit "Finished."

retR :: forall a b m. Monad m => (a -> m (Either b a)) 
retR = return . Right

withIO :: MonadIO m => IO a -> m (Either b ())
withIO ioAct = liftIO ioAct >> retR ()

forget = mkConst (Right ())

timeD :: (HasTime t s, Monad m) => Wire s e m a Double
timeD = fmap realToFrac time

