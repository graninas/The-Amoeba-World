module Misc.Id where

import World.Types

class Id i where
    getId :: i -> ItemId

isIdsEqual :: Id i => i -> i -> Bool
isIdsEqual item1 item2 = getId item1 == getId item2

invalidId :: ItemId
invalidId = -1