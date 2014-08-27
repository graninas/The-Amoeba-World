module Amoeba.Application.Game.Logic where

import Amoeba.Application.Game.Engine.GameWire
import Amoeba.View.Language

import qualified Amoeba.Application.Assets.GameFlow1 as GF1
import qualified Amoeba.Application.Assets.GameFlow2 as GF2

logic :: GameWire () ()
logic = GF1.gameNode TitleScreen



