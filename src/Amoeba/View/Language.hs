module Amoeba.View.Language where

import Amoeba.View.Output.Types

data GameNode = TitleScreen | Screen2 | Screen3 | Screen4
  deriving (Ord, Eq, Show, Enum)

data Command = Finish
             | Render
             | StartViewPointMoving ScreenPoint
             | ViewPointMoving      ScreenPoint
             | StopViewPointMoving  ScreenPoint
 deriving (Ord, Eq, Show)