module Amoeba.Application.Game.Engine.Runtime where

-- TODO: remove bad dependencies from View and GameLogic.
-- Needs generalization.
import Amoeba.View.Runtime
import Amoeba.GameLogic.Facade
import Amoeba.GameLogic.GameLogicAccessor
import Amoeba.GameStorage.Facade as GS

import Control.Monad.State (get, put, StateT(..))
import Control.Monad.State.Class
import Control.Monad (liftM)

-- This runtime can be chanded in the future.
data ViewRt = ViewRt { grtView :: View
                     , grtGameLogicAccessor :: GameLogicAccessor
                     }
type ViewTIO = StateT ViewRt IO

-- This runtime can be chanded in the future.
type GameStorageRt = GS.GameStorageAccessor
type GameStorageTIO = StateT GameStorageRt IO

-- This runtime can be chanded in the future.
type AIPlayerRt = GS.GameStorageAccessor
type AIPlayerTIO = StateT AIPlayerRt IO

viewRuntime = ViewRt

getView :: ViewTIO View
getView = liftM grtView get

putView :: View -> ViewTIO ()
putView view = modify (\rt -> rt { grtView = view })

getGameLogicAccessor :: ViewTIO GameLogicAccessor
getGameLogicAccessor = liftM grtGameLogicAccessor get

putGameLogicAccessor :: GameLogicAccessor -> ViewTIO ()
putGameLogicAccessor glAccessor = modify (\rt -> rt { grtGameLogicAccessor = glAccessor })

getStorageAccessor :: GameStorageTIO GameStorageRt
getStorageAccessor = get