module GameView.Items where

import World.World
import World.Items.Karyon
import World.Items.Stone
import World.Items.Canyon
import World.Items.Border
import World.Items.Plasma

import GameView.Render
import GameView.Color

import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Rect as SDL
import Control.Monad (void)

-- This is temporary solutions.

instance Render ActiveItem where
    render surf rect (MkActiveItem i) = void $ SDL.box surf rect (getColorByName . name $ i)

getColorByName "Karyon" = col1
getColorByName "KaryonFiller" = col2
getColorByName "Border" = col5
getColorByName "Plasma" = col3
getColorByName "Canyon" = col4
getColorByName "Stone" = col6
getColorByName n = error $ "Unknown name: " ++ n

    
        