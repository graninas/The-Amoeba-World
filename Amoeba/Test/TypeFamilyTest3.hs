{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

type Caption = String
type Name = String
type PlayerName = String


data PropertyToken a where
     IntProperty :: Name -> Int -> PropertyToken Int
     IntResource :: Name -> (Int, Int) -> PropertyToken (Int, Int)

instance Show (PropertyToken Int) where
    show (IntProperty _ i) = show i

instance Show (PropertyToken (Int, Int)) where
    show (IntResource _ i) = show i

class Prop a where
    type Out a :: *
    getProperty :: a -> Out a

instance Prop (PropertyToken a) where
    type Out (PropertyToken a) = a
    getProperty (IntProperty _ k) = k
    getProperty (IntResource _ k) = k



token1 = IntProperty "int" 10
token2 = IntResource "intResource" (10, 1000)
--tokens = [token1, token2]

main = do
    let res1 = getProperty (IntProperty "a" 10)
    let res2 = getProperty (IntResource "b" (20, 20))

    print res1
    print res2

    putStrLn "Ok."