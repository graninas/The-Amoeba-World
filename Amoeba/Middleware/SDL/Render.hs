module Middleware.SDL.Render where

import qualified Middleware.SDL.SDLFacade as SDL

clearScreen surf = do
    p <- SDL.mapRGB (SDL.surfaceGetPixelFormat surf) 0 100 0
    SDL.fillRect surf Nothing p
    
    
    