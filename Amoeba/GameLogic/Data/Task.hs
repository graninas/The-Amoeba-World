module GameLogic.Data.Task where

import qualified Data.Map as M
import Prelude hiding (null, lookup)

import GameLogic.Data.Types
import GameLogic.Data.Object
import Middleware.Math.Geometry

data Routine = Routine
  deriving (Show, Read, Eq)

data Action = Capture
  deriving (Show, Read, Eq)

data Target = TargetArea Bound
  deriving (Show, Read, Eq)
  
data Task = Task Target Action (Maybe Routine)
  deriving (Show, Read, Eq)
type Tasks = [Task]
type TaskMap = M.Map ObjectId Tasks


capture = Capture
