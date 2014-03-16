module GameLogic.Data.Types where

type ObjectType = Int
type ObjectId = Int
type PlayerId = Int

newtype Player = Player PlayerId
  deriving (Show, Read, Eq)

type Players = [Player]