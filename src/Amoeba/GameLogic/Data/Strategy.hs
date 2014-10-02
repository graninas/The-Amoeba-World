module Amoeba.GameLogic.Data.Strategy where

import qualified Data.Map as M
import Prelude hiding (null, lookup)

import Amoeba.GameLogic.Data.Object
import Amoeba.Middleware.Math.Geometry

data Target = TargetArea Bound
            | TargetObject ObjectId
  deriving (Show, Read, Eq)

data Task = CaptureTarget Target
  deriving (Show, Read, Eq)

data Strategy = ComplexStrategy [Strategy]
              | Strategy Task
              | EmptyStrategy
  deriving (Show, Read, Eq)
  
data Strategies = Strategies { humanStrategy :: Strategy
                             , ai1Strategy :: Strategy
                             }
  deriving (Show, Read, Eq)

emptyStrategies = Strategies EmptyStrategy EmptyStrategy
