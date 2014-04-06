module Application.Game.Logic where

import Application.Game.Engine.Types
import Application.Game.Engine.Core
import Application.Game.Runtime
import View.Color
import View.View
import GameLogic.Facade

import Middleware.FRP.NetwireFacade
import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State
import qualified Data.Map as M

type GameWire a b = GWire GameStateTIO a b


-- TODO
scale = 10
cellSide = 5

logic :: GameWire () ()
logic = render


getColorByPlayer pl | pl == dummyPlayer = white
getColorByPlayer pl | pl == player1 = green
getColorByPlayer pl | pl == player2 = blue

renderCell surf (p, Object _ _ pl _ _ _) = do
    let sdlRect = toSdlRect p
    SDL.box surf sdlRect (getColorByPlayer pl)

renderWorldMap surf w h wm = mapM_ (renderCell surf) (M.toList wm)

toSdlRect :: Point -> SDL.Rect
toSdlRect p = SDL.Rect x y cellSide cellSide
  where
    x = scale * pointX p
    y = scale * pointY p

-- TODO: make it safe in a type-level. Either or Maybe is needed.
render :: GameWire () ()
render = mkGen_ $ \_ -> do
    surf <- getSurface
    withLogError (clearScreen surf) "clearScreen: fillRect failed."
    (World wm _ w h _) <- getWorld
    withIO $ renderWorldMap surf w h wm
    withIO $ SDL.flip surf
    

{-
baseFill w@(World (WorldMap wm b) _ _)
         v@(View surf scr) = do
    SDL.fillRect surf Nothing nullColor
    
    let l = Map.toList wm
    mapM_ (renderCell v) l
    
    SDL.flip surf


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