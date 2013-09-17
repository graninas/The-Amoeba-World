module Test.Utils.Dummy where

import GameView.Render
import World.Id
import World.Player
import World.Descripted
import World.World

import Test.QuickCheck
import Control.Monad

data Dummy = Dummy Int
  deriving (Show, Read, Eq)
  
instance Render Dummy where
    render _ _ (Dummy i) = print i
    
instance Id Dummy where
    getId (Dummy i) = i

instance Descripted Dummy where
    description = show
    
instance Active Dummy where
    ownedBy _ = dummyPlayer
    activate = inactive
    name _ = "Dummy"

instance Arbitrary Dummy where
    arbitrary = liftM Dummy arbitrary