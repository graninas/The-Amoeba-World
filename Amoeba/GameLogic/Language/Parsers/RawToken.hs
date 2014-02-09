module GameLogic.Language.Parsers.RawToken where

type Caption = String
type Name = String
type PlayerName = String

data PropertyToken = IntProperty Name Int
                   | ObjectProperty Name RawToken
                   | CellsProperty Name [PropertyToken]
                   | CellProperty (Int, Int) RawToken
                   | IntResource Name (Int, Int)
  deriving (Show, Read, Eq)

data RawToken = Comment String
               | World Name [PropertyToken]
               | Item Name [PropertyToken]
               | Object Name PlayerName
               | EmptyToken
  deriving (Show, Read, Eq)
