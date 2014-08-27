{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Default
import Data.Maybe
import Data.Char
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Test.QuickCheck
import Test.QuickCheck.All

{-
instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b
-}

type MyMap = Map.Map Int String

data MyVal = Val { _name :: String
                 , _idt :: Int }
  deriving (Show, Read, Eq)
type MyMap2 = Map.Map Int MyVal

testMap1 = Map.fromList [(1, "ABC"), (2, "acvx"), (3, "87s"), (10, "IOU*^^")]
testMap2 = Map.fromList [(1, ["ABC", "CDE"]), (2, ["acvx", ""]), (3, ["87s", "}}{}{}{", "||||||"]), (10, ["IOU*^^"])]
testMap3 = Map.fromList [(1, Val "N1" 123123), (2, Val "N2" 1212), (100, Val "" 0), (10, Val "bvvvb" 6)]
testMap4 = Map.fromList [(50, Val "50" 50), (60, Val "60" 60)]

myVal1 = Val "ABC" 123

makeLenses ''MyVal

f1 :: MyMap -> MyMap
f1 = Map.insert 5 "5"
f2 = Map.lookup 3
f3 m = take 1 . drop 3 $ Map.keys m

t1 = testMap1 ^. folding f1         -- "ABCacvx87s5IOU*^^"
t2 = testMap1 ^. folding f2         -- "87s"
t3 = testMap1 ^.. folding f1        -- ["ABC","acvx","87s","5","IOU*^^"]
t4 = testMap1 ^.. folding f2        -- ["87s"]

u1 = testMap1 ^.  id                -- fromList [(1,"ABC"),(2,"acvx"),(3,"87s"),(10,"IOU*^^")]
u2 = testMap1 ^.. id                -- [fromList [(1,"ABC"),(2,"acvx"),(3,"87s"),(10,"IOU*^^")]]
u3 = Map.null $ testMap1 ^.  id     -- False
u4 = null     $ testMap1 ^.. id     -- False

a1 = testMap2 ^.  folded           -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
a2 = testMap2 ^.  traversed        -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
a3 = testMap2 ^.  traversed.folded -- "ABCCDEacvx87s}}{}{}{||||||IOU*^^"
a4 = testMap2 ^.  folded.traversed -- "ABCCDEacvx87s}}{}{}{||||||IOU*^^"
a5 = testMap2 ^.. folded           -- [["ABC","CDE"],["acvx",""],["87s","}}{}{}{","||||||"],["IOU*^^"]]
a6 = testMap2 ^.. traversed        -- [["ABC","CDE"],["acvx",""],["87s","}}{}{}{","||||||"],["IOU*^^"]]
a7 = testMap2 ^.. traversed.folded -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
a8 = testMap2 ^.. folded.traversed -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]

b1 = testMap2 ^. traverse               -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
b2 = testMap2 ^. traverse.folded        -- "ABCCDEacvx87s}}{}{}{||||||IOU*^^"
b3 = testMap2 ^. traverse.traversed     -- "ABCCDEacvx87s}}{}{}{||||||IOU*^^"
b4 = testMap2 ^. folded.traverse        -- "ABCCDEacvx87s}}{}{}{||||||IOU*^^"
b5 = testMap2 ^. traversed.traverse     -- "ABCCDEacvx87s}}{}{}{||||||IOU*^^"
b6  = testMap2 ^.. traverse             -- [["ABC","CDE"],["acvx",""],["87s","}}{}{}{","||||||"],["IOU*^^"]]
b7  = testMap2 ^.. traverse.folded      -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
b8  = testMap2 ^.. traverse.traversed   -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
b9  = testMap2 ^.. folded.traverse      -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]
b10 = testMap2 ^.. traversed.traverse   -- ["ABC","CDE","acvx","","87s","}}{}{}{","||||||","IOU*^^"]


d = ["abcdef", "cde", "uvx"]

prop1, prop2 :: Fold [String] Char
prop1 = traverse . ix 4
prop2 = traverse . ix 100
prop3 :: Traversal' [String] String
prop3 = ix 1
prop1' :: Traversal' [String] Char
prop1' = traverse . ix 4

check1 = prop1 . to ('a' ==)
check2 = prop2 . to ('a' ==)
check3 = prop1 . to isDigit
check4 = prop3 . to (not . null)
checks1 = (check1, check2)
checks2 = [check1, check2, check3]
checks3 = [check3,          check4]
checks4 = [check3 . to not, check4]
-- broken with lens-4.0.3
--checks5 = check3 . to not <> check4     -- needs a Monoid instance for Accessor
--checks6 = check3          <> check4     -- needs a Monoid instance for Accessor

isJustTrue (Just x) = x
isJustTrue Nothing = False

q1  = has prop1 d                                  -- True
q2  = d ^? prop1                                   -- Just 'e'
q3  = d ^? check1                                  -- Just False
q4  = d ^? check2                                  -- Nothing
q5  = allOf both (isJust . (d ^?)) checks1         -- False
q6  = anyOf both (isJust . (d ^?)) checks1         -- True
q7  = allOf traverse (isJust . (d ^?)) checks2     -- False
q8  = anyOf traverse (isJust . (d ^?)) checks2     -- True
q9  = allOf traverse (isJustTrue . (d ^?)) checks3 -- False
q10 = anyOf traverse (isJustTrue . (d ^?)) checks3 -- True
q11 = allOf traverse (isJustTrue . (d ^?)) checks4 -- True
q12 = anyOf traverse (isJustTrue . (d ^?)) checks4 -- True
-- broken with lens-4.0.3
--q13 = isJustTrue . (d ^?) $ checks5                -- True
--q14 = isJustTrue . (d ^?) $ checks6                -- False

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests