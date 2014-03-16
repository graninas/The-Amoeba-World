module GameLogic.Language.Translation.Triggers where

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

-- TODO: Generalize by type class Named.
onProp :: String -> PropertyToken -> Bool
onProp name (IntProperty n _)          = n == name
onProp name (ObjectProperty n _)       = n == name
onProp name (CellsProperty n _)        = n == name
onProp name (CellProperty n _ _)       = n == name
onProp name (IntResourceProperty n _ ) = n == name

