module Application.Storage.CellularNetLoader where

import CellularNet.Net

import Application.Game.Engine.Runtime
import qualified Middleware.Tracing.Log as Log
import Middleware.Tracing.ErrorHandling

loadNet = viewFastNet 0