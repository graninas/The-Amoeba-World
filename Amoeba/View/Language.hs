module View.Language where

import Data.Word (Word16)

data GameNode = Screen1 | Screen2 | Screen3 | Screen4
  deriving (Ord, Eq, Show, Enum)

data Command = Finish
             | Render
             | Update
             | SwitchNode GameNode
             | StartViewPointMoving Word16 Word16
             | ViewPointMoving Word16 Word16
             | StopViewPointMoving Word16 Word16
  deriving (Ord, Eq, Show)