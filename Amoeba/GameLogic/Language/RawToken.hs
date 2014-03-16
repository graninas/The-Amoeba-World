module GameLogic.Language.RawToken where

type Caption = String
type Name = String
type PlayerName = String

data PropertyToken = IntProperty Name Int
                   | ObjectProperty Name RawToken
                   | CellsProperty Name [PropertyToken]
                   | CellProperty Name (Int, Int) RawToken
                   | IntResourceProperty Name (Int, Int)
  deriving (Show, Read, Eq)

data RawToken = CommentToken String
               | WorldToken Name [PropertyToken]
               | ItemToken Name [PropertyToken]
               | ObjectToken Name PlayerName
               | EmptyToken
  deriving (Show, Read, Eq)
