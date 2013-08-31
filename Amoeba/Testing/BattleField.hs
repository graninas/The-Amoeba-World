module BattleField where

import ActiveItem
import Fort
import Factory
    
    
items :: ActiveItems
items = [packItem fort, packItem factory, packItem brokenFort]

