module GameView.Render where

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

data Scale = ScaleNormal | ScaleSmall1 | ScaleSmall2
  deriving (Show, Read, Eq)

data View = View { viewPlayer :: Player
                 , viewCenter :: Point
                 , viewScale :: Scale
                 }  
  deriving (Show, Read, Eq)

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