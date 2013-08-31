{-# LANGUAGE ExistentialQuantification, TypeFamilies, MultiParamTypeClasses #-}

module FieldItem where

class Item a where
    animate :: Int -> a -> a

class Descripted d where
    description :: d -> String
    
class Item a => Active awhere
    mutate :: a -> md -> a
    activate :: a -> [md]
    defMd :: a -> md
s






























{- Work 2
data family FieldItem a

class Item a where
    animate :: Int -> a -> a
    activate :: a -> [Int]

data ActiveItem = forall a. Item a => MkActiveItem a
type ActiveItems = [ActiveItem]



packItem :: forall a. Item a => a -> ActiveItem
packItem = MkActiveItem
-}









































{- Work1
data family FieldItem a

class Item a where
    animate :: Int -> a -> a
    activate :: a -> [Int]
-}