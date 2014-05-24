module View.Runtime where

import qualified Middleware.SDL.SDLFacade as SDL
import Data.Word (Word16)

type ViewPoint = (Int, Int)
type ScreenPoint = (Word16, Word16)

data Screen = Screen { width :: Int
                     , height :: Int
                     , depth :: Int }
    deriving (Show, Read, Eq)

type ViewSurface = SDL.Surface
data View = View { viewSurface :: ViewSurface
                 , viewScreen :: Screen
                 , viewCaption :: String
                 , viewVirtualPlain :: ViewPoint
                 , viewVirtualPlainShift :: Maybe (ViewPoint, ViewPoint)
                 }
                 
toViewPoint :: ScreenPoint -> ViewPoint
toViewPoint (x, y) = (fromIntegral x, fromIntegral y)