module View.Language where

data Screen = Screen { width :: Int
                     , height :: Int
                     , depth :: Int }
    deriving (Show, Read, Eq)


