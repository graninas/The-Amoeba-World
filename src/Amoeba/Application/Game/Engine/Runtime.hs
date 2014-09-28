module Amoeba.Application.Game.Engine.Runtime where

import Amoeba.View.ViewAccessor
import Amoeba.GameLogic.GameLogicAccessor

import Control.Monad.State (get, StateT(..))
import Control.Monad (liftM)
import Control.Applicative

-- This runtime can be chanded in the future.
data ViewRt = ViewRt { grtViewAccessor :: ViewAccessor
                     , grtGameLogicAccessor :: GameLogicAccessor
                     }
type ViewTIO = StateT ViewRt IO

mkViewRuntime :: ViewAccessor -> GameLogicAccessor -> ViewRt
mkViewRuntime = ViewRt

getViewAccessor :: ViewTIO ViewAccessor
getViewAccessor = liftM grtViewAccessor get

getViewGameLogicAccessor :: ViewTIO GameLogicAccessor
getViewGameLogicAccessor = liftM grtGameLogicAccessor get

getAccessors :: ViewTIO (GameLogicAccessor, ViewAccessor)
getAccessors = (,) <$> getViewGameLogicAccessor <*> getViewAccessor

-- This runtime can be chanded in the future.
type GameStorageRt = GameLogicAccessor
type GameStorageTIO = StateT GameStorageRt IO

getStorageGameLogicAccessor :: GameStorageTIO GameLogicAccessor
getStorageGameLogicAccessor = get

-- This runtime can be chanded in the future.
type AIPlayerRt = GameLogicAccessor
type AIPlayerTIO = StateT AIPlayerRt IO
