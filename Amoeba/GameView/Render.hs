{-# LANGUAGE ExistentialQuantification #-}

module GameView.Render where

import World.Id

class Id r => Render r where
    render :: r -> IO ()
--    domain :: r -> Bound

data Renderable = forall r. Render r => MkRenderable r
type Renderables = [Renderable]

packRenderable :: Render r => r -> Renderable
packRenderable = MkRenderable

instance Id Renderable where
    getId (MkRenderable r) = getId r

instance Eq Renderable where
    r1 == r2 = isIdsEqual r1 r2
