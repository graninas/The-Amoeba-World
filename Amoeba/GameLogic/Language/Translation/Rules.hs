module GameLogic.Language.Translation.Rules where

import GameLogic.Language.Translation.Triggers
import GameLogic.Language.Translation.Actions
import GameLogic.Language.Scheme

scheme = [ onComment /> skip
         , onEmpty /> skip
         , onItem /> addItem [ onProp energy <-/> makeEnergy
                             , onProp durability <-/> makeDurability
                             , onProp lifebound <-/> makeLifebound
                             ]
         , onWorld /> setupWorld [ onProp width /> setWidth
                                 , onProp height /> setHeight
                                 , onProp cells /> setCells
                                 ]
         ]