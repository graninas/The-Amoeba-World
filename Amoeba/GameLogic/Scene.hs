module GameLogic.Scene where

import GameView.Render
import GameView.SceneGraph
import GameView.View
import GameView.Items
import World.Player
import World.World
import World.Geometry
import World.WorldMap

import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Rect as SDL
import qualified Graphics.UI.SDL.Color as SDL
import qualified Graphics.UI.SDL.Types as SDL
import Data.Monoid
import qualified Data.Map as Map

-- TODO
scene, menu :: SceneGraph
scene = {-under baseFill $-} under rWorld $ rFrame `under` rStats
menu = under scene $ rStartNewGame <> rQuit

rFrame = undefined
rStats = undefined
rStartNewGame = undefined
rQuit = undefined
rWorld = undefined

-- This is temporary solutions.

scale = 5
translationX = 300
translationY = 100

toSdlRect :: Point -> SDL.Rect
toSdlRect p = SDL.Rect x y w h
  where
    x = (scale * pointX p) + translationX
    y = (scale * pointY p) + translationY
    w = scale
    h = scale

renderCell _ (_, []) = return ()
renderCell v@(View surf (Screen w h)) c@(p, its) = render surf (toSdlRect p) (head its)

baseFill w@(World (WorldMap wm b) _ _)
         v@(View surf scr) = do
    nullColor <- SDL.mapRGB (SDL.surfaceGetPixelFormat surf) 0 0 0
    SDL.fillRect surf Nothing nullColor
    
    let l = Map.toList wm
    mapM_ (renderCell v) l
    
    SDL.flip surf


renderSceneGraph :: SceneGraph -> World -> IO ()
renderSceneGraph _ w = getView >>= baseFill w
  
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
    