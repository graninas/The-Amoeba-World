module GameLogic.Language.Translating.Rules where

import GameLogic.Language.Translating.Triggers
import GameLogic.Language.Translating.Actions
import GameLogic.Language.Scheme

scheme = [ onComment /> skip
         , onEmpty /> skip
         , onItem /> addItem
         , onWorld /> setupWorld [ onProp width /> setWidth
                                 , onProp height /> setHeight
                                 , onProp cells /> setCells
                                 ]
         ]