module CellularNet.View where

import View.Color
import View.Runtime
import CellularNet.Net hiding (stepNet)

import qualified Middleware.SDL.SDLFacade as SDL
import qualified Middleware.SDL.Render as SDL
import qualified Middleware.Tracing.Log as Log
import Middleware.Tracing.ErrorHandling

import qualified Data.Map as M

scale = 10
cellSide = 7
shiftX = -70
shiftY = -70

toSdlRect :: ViewPoint -> Pos -> SDL.Rect
toSdlRect (planeX, planeY) (x, y) = SDL.Rect x' y' (x' + cellSide) (y' + cellSide)
  where
    x' = x * scale + planeX
    y' = y * scale + planeY

toWord8Pixel (r, g, b) = toSdlPixel (fi r, fi g, fi b)
  where fi = fromIntegral

getNeuronColor e    = toWord8Pixel (0, e * 10 + 100, 0)
getModulatorColor i | i == 0 = toWord8Pixel (0, 0, 0)
                    | i < 0  = toWord8Pixel (0, 0, i * 10 + 200)
                    | i > 0  = toWord8Pixel (i * 10 + 200, 0, 0)

-- TODO: replace this specific function by linear. 
(+!) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
(-!) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

renderCell surf plane (pos, Neuron e) = do
    let col = getNeuronColor e
    let neuron = toSdlRect plane pos
    withLogError (SDL.rectangle surf neuron col) "render neuron: rectangle failed."
    return ()

renderCell surf plane (pos, Modulator i) = do
    let col = getModulatorColor i
    let modulator = toSdlRect plane pos
    withLogError (SDL.rectangle surf modulator col) "render modulator: rectangle failed."
    return ()

renderNetMap surf plane net = mapM_ (renderCell surf plane) (M.toList net)

renderBorders surf = do
    let rect = SDL.Rect 1 1 638 478 -- TODO: use screen data to calculate borders
    SDL.rectangle surf rect (toSdlPixel white)

renderNet (View surf _ _ vPlane mbShift) (FastNet _ _ net) = renderNet' (shiftPlane mbShift)
  where
    renderNet' plane = do
        SDL.clearScreen surf
        renderBorders surf
        renderNetMap surf plane net
        SDL.flip surf
    shiftPlane Nothing = vPlane
    shiftPlane (Just (p1, p2)) = vPlane +! p2 -! p1
