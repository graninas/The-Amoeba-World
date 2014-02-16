module GameLogic.Language.Translating.Scheme where

import GameLogic.Language.Translating.Triggers
import GameLogic.Language.Translating.Actions



scheme = [ onComment /> skip
         , onEmpty /> skip
         , onItem /> addItem
         ]