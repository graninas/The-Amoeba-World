module GameView.SceneGraph where

import GameView.Render

import Data.Monoid

data SceneGraph = SceneGraph { sceneGraphLayer :: Renderables
                             , sceneGraphBasement :: SceneGraph }
                | Basement

instance Eq SceneGraph where
    Basement == Basement = True
    Basement == _ = False
    _ == Basement = False
    (SceneGraph l1 b1) == (SceneGraph l2 b2) = (l1 == l2) && (b1 == b2)
    

instance Monoid SceneGraph where
    mempty = Basement
    Basement `mappend` sc = sc
    sc `mappend` Basement = sc
    (SceneGraph l1 b1) `mappend` (SceneGraph l2 b2) = let
        ls = l1 ++ l2
        bs = b1 `mappend` b2
        in SceneGraph ls bs

under :: SceneGraph -> SceneGraph -> SceneGraph
sc1 `under` (SceneGraph l b) = SceneGraph l (b <> sc1) 
sc1 `under` Basement = sc1 