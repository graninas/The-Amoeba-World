{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import Data.Monoid

import GameView.Render
import GameView.SceneGraph
import World.Id
import Test.Dummy

instance Arbitrary SceneGraph where
    arbitrary = oneof [ return Basement
                      , liftM2 SceneGraph (return l) arbitrary ]
        where
            l = [packRenderable (Dummy 10)]

instance Show SceneGraph where
    show Basement = "Basement {}"
    show (SceneGraph l b) = "SceneGraph { sceneGraphLayer=" ++ show l
                         ++ ", sceneGraphBasement="
                         ++ show b ++ " }"

instance Show Renderable where
    show (MkRenderable r) = "Renderable <" ++ (show . getId $ r) ++ ">"

prop_monoidLaw1 sc = mappend mempty sc == sc
    where
        types = sc :: SceneGraph

prop_monoidLaw2 sc = mappend sc mempty == sc
    where
        types = sc :: SceneGraph

prop_monoidLaw3 sc1 sc2 sc3 = mappend sc1 (mappend sc2 sc3) == mappend (mappend sc1 sc2) sc3
    where
        types = [sc1, sc2, sc3] :: [SceneGraph]

prop_monoidLaw4 sc1 sc2 sc3 = mconcat scs == foldr mappend mempty scs
    where
        scs = [sc1, sc2, sc3] :: [SceneGraph]


runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."