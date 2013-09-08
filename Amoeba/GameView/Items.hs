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

reduceRect (SDL.Rect x y w h) = SDL.Rect (x+1) (y+1) (w-1) (h-1)

instance Render ActiveItem where
    render surf rect (MkActiveItem i) = do
        let col = getColorByName $ name i
        let shape = reduceRect rect
        void $ SDL.box surf shape col

getColorByName "Karyon" = col1
getColorByName "KaryonFiller" = col2
getColorByName "Border" = col5
getColorByName "Plasma" = col3
getColorByName "Canyon" = col4
getColorByName "Stone" = col6
getColorByName n = error $ "Unknown name: " ++ n

    
        