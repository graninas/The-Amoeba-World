module Application.Game.Logic where

import Application.Game.Engine.GameWire
import View.Language

import qualified Application.Assets.GameFlow1 as GF1
import qualified Application.Assets.GameFlow2 as GF2

logic :: GameWire () ()
logic = GF2.gameNode Screen1



