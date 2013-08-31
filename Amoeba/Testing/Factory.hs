{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Factory where

import FieldItem


    
data Factory = Factory Int Int Int
data FactoryAction = Produce | Ok
    deriving (Show)


instance Item Factory where
    animate n (Factory i1 i2 i3) = Factory (i1+i2) (i2+i3) (i3+i1)
    
instance Descripted FactoryAction where
    description = show
    
instance Active Factory FactoryAction where
    type Modificator FactoryAction = FactoryAction
    mutate (Factory i1 i2 i3) Produce = Factory 10 10 10
    activate (Factory i1 _ _) = [Produce, Produce]
    defMd _ = Ok

factory :: Factory
factory = Factory 0 1 2













































{- Work 2

data Factory = Factory Int Int Int
data FactoryAction = Produce

data instance FieldItem Factory = FIFactory Factory
    
instance Item (FieldItem Factory) where
    animate n (FIFactory f) = FIFactory $ animate n f
    activate (FIFactory f) = activate f


instance Item Factory where
    animate n (Factory i1 i2 i3) = Factory (i1+i2) (i2+i3) (i3+i1)
    activate (Factory i1 _ _) = [i1]

factory :: Factory
factory = Factory 0 1 2

-}


















{- Work1
data Factory = Factory Int Int Int
data FactoryAction = Produce

data instance FieldItem Factory = FIFactory Factory
    
instance Item (FieldItem Factory) where
    animate n (FIFactory (Factory i1 i2 i3)) = FIFactory (Factory (i1+i2) (i2+i3) (i3+i1))
    activate (FIFactory (Factory i1 _ _)) = [i1]
-}