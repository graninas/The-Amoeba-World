{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

module World.Test where

class A i where
  a :: i -> Bool
  
class B j where
  b :: j -> Int
  
  
