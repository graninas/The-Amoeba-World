module Application.Game.Logic where

import Application.Game.Engine.GameWire
import View.Language

import qualified Application.Assets.GameFlow1 as GF1

logic :: GameWire () ()
logic = GF1.gameNode Screen1



