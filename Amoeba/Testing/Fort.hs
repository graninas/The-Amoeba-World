{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Fort where

import FieldItem


data Fort = Fort String Int
          | BrokenFort String

data FortAction = Repare Int | A2 Char | Ok
    deriving (Show)

instance Item Fort where
    animate 0 (Fort s i) = BrokenFort $ s ++ show i
    animate n f = f 

instance Descripted FortAction where
    description = show
    
instance Active Fort FortAction where
    type Modificator FortAction = FortAction
    mutate (BrokenFort {}) (Repare {}) = Fort "Repared" 10
    mutate f _ = f
    activate (Fort s i) = [Repare 10]
    activate _ = []
    defMd _ = Ok


fort :: Fort
fort = Fort "This is Fort." 100

brokenFort :: Fort
brokenFort = BrokenFort "This is Broken Fort."



































{- Work2
data Fort = Fort String Int
          | BrokenFort String
data FortAction = A1 | A2 Char

data instance FieldItem Fort = FIFort Fort
    
instance Item (FieldItem Fort) where
    animate n (FIFort f) = FIFort (animate n f)
    activate (FIFort f) = activate f

instance Item Fort where
    animate 0 (Fort s i) = BrokenFort $ s ++ show i
    animate n f = f 
    activate (Fort s i) = [i]
    

fort :: Fort
fort = Fort "This is Fort." 100

-}





























{- Work1
data Fort = Fort String Int
          | BrokenFort String
data FortAction = A1 | A2 Char

data instance FieldItem Fort = FIFort Fort
    
instance Item (FieldItem Fort) where
    animate n (FIFort (Fort s i)) = FIFort (BrokenFort $ s ++ show i)
    animate n f = f 
    activate (FIFort (Fort s i)) = [i]
-}