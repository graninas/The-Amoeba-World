module CellularNet.RuntimeSt where

import CellularNet.Net

class Monad m => RuntimeSt m where
  getData :: m FastNet
  putData :: FastNet -> m ()

