module GameLogic.Language.Translation.Rules where

import GameLogic.Language.Translation.Triggers
import GameLogic.Language.Translation.Actions
import GameLogic.Language.Scheme

scheme = [ onComment /> skip
         , onEmpty /> skip
         , onItem /> addItem
         , onWorld /> setupWorld [ onProp width /> setWidth
                                 , onProp height /> setHeight
                                 , onProp cells /> setCells
                                 ]
         ]