module GameLogic.Language.Translating.Triggers where

import GameLogic.Language.RawToken

onComment :: RawToken -> Bool
onComment (Comment _) = True
onComment _ = False

onEmpty EmptyToken = True
onEmpty _ = False

onItem (Item n props) = True
onItem _ = False

onWorld (World name props) = True
onWorld _ = False

onProp :: String -> PropertyToken -> Bool
onProp name (IntProperty n _)    | n == name = True
onProp name (ObjectProperty n _) | n == name = True
onProp name (CellsProperty n _)  | n == name = True
onProp name (CellProperty n _ _) | n == name = True
onProp name (IntResource n _ )   | n == name = True
onProp _ _ = False
