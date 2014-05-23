module View.NetView where

import View.Color
import View.Runtime
import CellularNet.Net

import qualified Middleware.SDL.SDLFacade as SDL
import qualified Middleware.SDL.Render as SDL
import qualified Middleware.Tracing.Log as Log
import Middleware.Tracing.ErrorHandling

import qualified Data.Map as M

scale = 10
cellSide = 7

toSdlRect :: Pos -> SDL.Rect
toSdlRect (x, y) = SDL.Rect x' y' (x' + cellSide) (y' + cellSide)
  where
    x' = x * scale
    y' = y * scale

toWord8Pixel (r, g, b) = toSdlPixel (fi r, fi g, fi b)
  where fi = fromIntegral

getNeuronColor e    = toWord8Pixel (0, e * 10 + 100, 0)
getModulatorColor i = toWord8Pixel (0, 0, i * 10 + 100)

renderCell surf (pos, Neuron e) = do
    let col = getNeuronColor e
    let neuron = toSdlRect pos
    withLogError (SDL.rectangle surf neuron col) "render neuron: rectangle failed."
    return ()

renderCell surf (pos, Modulator i) = do
    let col = getModulatorColor i
    let modulator = toSdlRect pos
    withLogError (SDL.rectangle surf modulator col) "render modulator: rectangle failed."
    return ()

renderNetMap surf net = mapM_ (renderCell surf) (M.toList net)

renderBorders surf = do
    let rect = SDL.Rect 1 1 638 478
    SDL.rectangle surf rect (toSdlPixel white)

renderNet (View surf screen _) (FastNet _ _ net) = do
    SDL.clearScreen surf
    renderBorders surf
    renderNetMap surf net
    SDL.flip surf
