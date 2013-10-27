module GameLogic.Types where

type ItemId = Int
type PlayerId = Int

type Speed = Int
type Energy = Int
type EnergyCapacity = Energy
type Durability = Int
type MaxDurability = Durability
type Age = Int
type MaxAge = Age
type Power = Int

data Named = Named String
  deriving (Show, Read, Eq)


-- Maybe ovekill?
class (Ord a, Num a) => ZeroOrd a where
  zeroCompare :: a -> Ordering

instance ZeroOrd Int where
  zeroCompare i = compare i 0
  