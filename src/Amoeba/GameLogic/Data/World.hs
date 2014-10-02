module Amoeba.GameLogic.Data.World where

import qualified Control.Lens as L
import qualified Data.Map as M
import Prelude hiding (null, lookup)

import Amoeba.GameLogic.Data.Object
import Amoeba.Middleware.Math.Geometry

data Effect = Effect
  deriving (Show, Read, Eq)
type Effects = [Effect]
type EffectMap = M.Map ObjectId Effects


type WorldMap = M.Map Point Object
data World = World { worldMap :: WorldMap
                   , worldEffects :: EffectMap
                   , width :: Int
                   , height :: Int
                   , defaultCell :: Maybe Object
                   }
  deriving (Show, Read, Eq)

emptyWorld = World M.empty M.empty 0 0 Nothing

insertObject point object w@(World wm _ _ _ _) = w { worldMap = M.insert point object wm }

worldMapSize (World wm _ _ _ _) = M.size wm


