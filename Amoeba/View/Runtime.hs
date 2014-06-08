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

-- TODO: replace this specific function by linear.
(+!) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
(-!) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)