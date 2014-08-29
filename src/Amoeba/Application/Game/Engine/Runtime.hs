module Amoeba.Application.Game.Engine.Runtime where

import Amoeba.Application.Game.GameState

-- TODO: remove bad dependency from View
import Amoeba.View.Runtime
import Amoeba.Middleware.Config.Facade

import Control.Monad.State (get, put, StateT(..))
import Control.Monad.State.Class
import Control.Monad (liftM)

-- For hack:
import qualified Control.Concurrent.STM.TVar as STM


data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtData :: GameState
                     }

type GameStateTIO = StateT GameRt IO

runtime = GameRt

getConfiguration :: GameStateTIO Configuration
getConfiguration = liftM grtConfiguration get

putConfiguration :: Configuration -> GameStateTIO ()
putConfiguration cfg = modify (\rt -> rt { grtConfiguration = cfg })

getView :: GameStateTIO View
getView = liftM grtView get

putView :: View -> GameStateTIO ()
putView view = modify (\rt -> rt { grtView = view })

getData :: GameStateTIO GameState
getData = liftM grtData get

putData :: GameState -> GameStateTIO ()
putData dat = modify (\rt -> rt { grtData = dat })



-- TODO: HACK: FIXME: needs deep generalization of looping mechanism.
-- Will do it later. Now, this is a HACK.
type GameStorageRt = STM.TVar GameState
type GameStorageTIO = StateT GameStorageRt IO