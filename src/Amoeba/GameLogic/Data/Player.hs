module Amoeba.GameLogic.Data.Player where

data Player = Human
            | AI1
  deriving (Show, Read, Eq)
  
humanPlayer = Human
ai1Player = AI1