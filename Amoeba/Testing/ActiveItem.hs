{-# LANGUAGE ExistentialQuantification, TypeFamilies, MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts #-} -- for '()' in '(Mutable a ())'
{-# LANGUAGE FlexibleInstances #-} -- for '()' in '(Item a) => Mutable a ()'


module ActiveItem where

import FieldItem

data ActiveItem = forall a md. (Active a md) => MkActiveItem a md
type ActiveItems = [ActiveItem]

packItem :: forall a md. (Active a md) => a -> ActiveItem
packItem i = MkActiveItem i (defMd i)