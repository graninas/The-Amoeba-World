module GameLogic.Language.Translating.Runtime where

import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E


data TransRt = TransRt { trtNextId :: State TransRt Int
                       , trtItemMap :: M.Map String String -- TODO
                       , trtWorldConstructor :: String -- TODO
                       , trtLog :: [String]
                       }

type Trans a = EitherT String (State TransRt) a


log s = do
    ctx <- get
    let newCtx = ctx { trtLog = (trtLog ctx ++ [s]) }
    put newCtx
    

initialRt idCounter = TransRt idCounter M.empty "Empty" []