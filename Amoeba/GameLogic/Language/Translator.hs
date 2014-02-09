module GameLogic.Language.Translator where

import qualified GameLogic.Language.Parsers.RawParser as RP
import GameLogic.Runtime.World

{-
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
-}


translateToWorld w [] = Left "No data passed."
translateToWorld w (EmptyToken : ts) = translateToWorld w ts
translateToWorld w ts = undefined


toWorld rawString = do
    ts <- RP.parseRawTokens rawString
    translateToWorld ts