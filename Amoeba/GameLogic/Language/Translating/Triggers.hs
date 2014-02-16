module GameLogic.Language.Translating.Triggers where

import GameLogic.Language.RawToken


onComment (Comment _) = True
onComment _ = False

onEmpty EmptyToken = True
onEmpty _ = False

onItem (Item n props) = True
onItem _ = False

onWorld (World name props) = True
onWorld _ = False