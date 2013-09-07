module GameLogic.Scene where

import GameView.Render
import GameView.SceneGraph
import GameView.View
import World.Player
import World.World
import World.Geometry

import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Rect as SDL
import qualified Graphics.UI.SDL.Color as SDL
import qualified Graphics.UI.SDL.Types as SDL
import Data.Monoid
import Data.Word
import Data.Bits
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

-- From here, for tests: https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8 .|.
                            255)
  where fi = fromIntegral

scale = 10

renderCell v@(View surf (Screen w h)) c@(p, its) = undefined
    

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
    