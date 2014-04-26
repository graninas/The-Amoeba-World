module View.Runtime where

import qualified Middleware.SDL.SDLFacade as SDL

data Screen = Screen { width :: Int
                     , height :: Int
                     , depth :: Int }
    deriving (Show, Read, Eq)

type ViewSurface = SDL.Surface
data View = View { viewSurface :: ViewSurface
                 , viewScreen :: Screen
                 , viewCaption :: String
                 }