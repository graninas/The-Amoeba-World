module Test.Utils.WorldArfData where

import GameLogic.Language.RawToken
import qualified GameLogic.Language.Scheme as S

-- 'ARF' stands for 'Amoeba Raw File' or 'Amoeba Raw Format' if you wish.

items1 = ("Items1", "./Data/Raws/Items.arf",
         Right [ CommentToken " General items", EmptyToken
               , ItemToken S.karyon [ IntResource S.lifebound (0,5000)
                                    , IntResource S.durability (100,100)
                                    , IntResource S.energy (300,2000)]
               , EmptyToken, CommentToken " Conductor"
               , ItemToken S.conductor [IntResource S.lifebound (0,1000),IntResource S.durability (100,100),IntResource S.energy (0,100)]])

items2 = ("Items2", "./Data/Raws/Item.arf",
         Right [ItemToken S.karyon [IntResource S.lifebound (0,5000), IntResource S.durability (100,100), IntResource S.energy (300,2000)]])

world1 = ("World1", "./Data/Raws/World1.arf",
         Right [ CommentToken " World definition file"
               , EmptyToken
               , WorldToken "Pandora" [ IntProperty S.width 20, IntProperty S.height 20, ObjectProperty S.defaultCell (ObjectToken S.empty S.player0)
                                      , CellsProperty S.cells [ CellProperty S.cell (10, 10) (ObjectToken S.karyon S.player1)
                                                              , CellProperty S.cell (9, 9) (ObjectToken S.plasma S.player1)]] ])
world2 = ( "World2"
         , "./Data/Raws/World2.arf"
         , undefined )
world3 = ( "World3"
         , "./Data/Raws/World3.arf"
         , undefined )