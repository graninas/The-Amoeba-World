{-# LANGUAGE ExistentialQuantification #-}

module GameView.Render where

import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Rect as SDL
import qualified Graphics.UI.SDL.Color as SDL
import qualified Graphics.UI.SDL.Types as SDL

import World.Id

class Id r => Render r where
    render :: SDL.Surface -> SDL.Rect -> r -> IO ()
--    domain :: r -> Bound

data Renderable = forall r. Render r => MkRenderable r
type Renderables = [Renderable]

packRenderable :: Render r => r -> Renderable
packRenderable = MkRenderable

instance Id Renderable where
    getId (MkRenderable r) = getId r

instance Eq Renderable where
    r1 == r2 = isIdsEqual r1 r2
