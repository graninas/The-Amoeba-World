module Amoeba.GameLogic.Language.Translation.Rules where

import Amoeba.GameLogic.Language.Translation.Triggers
import Amoeba.GameLogic.Language.Translation.Actions
import Amoeba.GameLogic.Language.Scheme

scheme = [ onComment /> skip
         , onEmpty /> skip
         , onItem /> addItem
         , onWorld /> setupWorld [ onProp width /> setWidth
                                 , onProp height /> setHeight
                                 , onProp cells /> setCells
                                 ]
         ]