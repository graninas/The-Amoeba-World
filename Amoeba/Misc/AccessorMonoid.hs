module Misc.AccessorMonoid where

import Data.Monoid
import Control.Lens

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b