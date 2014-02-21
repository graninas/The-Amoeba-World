module GameLogic.Language.Translating.Triggers where

import GameLogic.Language.RawToken

onComment :: RawToken -> Bool
onComment (CommentToken _) = True
onComment _ = False

onEmpty EmptyToken = True
onEmpty _ = False

onItem (ItemToken n props) = True
onItem _ = False

onWorld (WorldToken name props) = True
onWorld _ = False

onProp :: String -> PropertyToken -> Bool
onProp name (IntProperty n _)    | n == name = True
onProp name (ObjectProperty n _) | n == name = True
onProp name (CellsProperty n _)  | n == name = True
onProp name (CellProperty n _ _) | n == name = True
onProp name (IntResource n _ )   | n == name = True
onProp _ _ = False
