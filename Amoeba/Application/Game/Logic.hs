module Application.Game.Logic where

import Application.Game.Engine.Types
import Application.Game.Runtime
import GameLogic.Data.Facade
import View.Color
import View.View

import Middleware.FRP.NetwireFacade
import Middleware.Tracing.ErrorHandling

import Control.Monad.State
import qualified Middleware.SDL.SDLFacade as SDL

type GameWire a b = GWire GameStateTIO a b

logic :: GameWire () ()
logic = render <|> mkConst (Right ())

-- TODO: make it safe in a type-level. Either or Maybe is needed.
render :: GameWire () ()
render = mkGen_ $ \_ -> do
    (World wm _ w h _) <- getWorld
    surf <- getSurface
    liftIO $ withLogError (clearScreen surf) "clearScreen: fillRect failed."
    return $ Right ()

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