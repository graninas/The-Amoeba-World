{-# LANGUAGE ExistentialQuantification #-}

module GameView.Render where

import World.Id
import World.World
import World.Player
import World.Geometry
import Middleware.Wire
import Application.GameFlow

import Control.Monad
import Control.Monad.State
import Control.Wire
import Graphics.UI.SDL
import Prelude hiding ((.), id)
import Data.Monoid

data Scale = Scale Float
  deriving (Show, Read, Eq)

data View = View { viewPlayer :: Player
                 , viewCenter :: Point
                 , viewScale :: Scale
                 }
  deriving (Show, Read, Eq)


class Id r => Render r where
    rend :: r -> IO ()
--    domain :: a -> Bound

data Renderable = forall r. Render r => MkRenderable r
type Renderables = [Renderable]

packRenderable :: Render r => r -> Renderable
packRenderable = MkRenderable

instance Id Renderable where
    getId (MkRenderable r) = getId r

instance Eq Renderable where
    r1 == r2 = isIdsEqual r1 r2

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

--    mappend mempty x = x
--    mappend x mempty = x
--    mappend x (mappend y z) = mappend (mappend x y) z
--    mconcat = foldr mappend mempty

under :: SceneGraph -> SceneGraph -> SceneGraph
sc1 `under` (SceneGraph l b) = SceneGraph l (b <> sc1) 
sc1 `under` Basement = sc1 

scene, menu :: SceneGraph
scene = under rWorld $ rFrame `under` rStats
menu = under scene $ rStartNewGame <> rQuit

rWorld = undefined
rFrame = undefined
rStats = undefined
rStartNewGame = undefined
rQuit = undefined

render :: WWire GameFlow GameFlow
render = mkFixM $ \dt gf -> do
    surface <- liftIO getVideoSurface
    
    
    return . Right $ gf
{-
render :: SDL.Surface -> SDLTTF.Font -> Frame -> IO ()
render screen font Frame{..} = do
  void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
    SDL.fillRect screen Nothing

  mapM_ renderAsteroid fAsteroids
  mapM_ (renderBounds . bounds) fBullets
  mapM_ renderPoint fParticles
  mapM_ renderUfo fUfo
  renderShip fShip

  scoreS <-
    SDLTTF.renderTextSolid font ("SCORE: " ++ show fScore)
      (SDL.Color 255 255 255)

  SDL.blitSurface scoreS Nothing screen (Just $ SDL.Rect 20 20 100 50)

  SDL.flip screen
-}