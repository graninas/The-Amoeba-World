module Application.Game.Logic where

import Application.Game.Engine.GameWire
import View.Language

import qualified Application.Assets.GameFlow1 as GF

logic :: GameWire () ()
logic = GF.gameNode Screen1



