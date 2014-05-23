module View.Language where

data GameNode = Screen1 | Screen2 | Screen3 | Screen4
  deriving (Ord, Eq, Show, Enum)

data Command = Finish
             | Render
             | Update
             | SwitchNode GameNode
 deriving (Ord, Eq, Show)