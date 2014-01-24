{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Data.Object where

import Control.Lens

import GameLogic.Data.Player


data Resource a = Resource { _stock :: a
                           , _capacity :: Maybe a }
  deriving (Show, Read, Eq)
  
type IntResource = Resource Int

data Object = Object {
                        -- Properties:
                         _objectId :: Int               -- static property
                       , _objectType :: Int             -- predefined property
                       
                       -- Runtime properties, resources:
                       , _ownership :: Player           -- runtime property... or can be effect!

                       , _lifebound  :: IntResource    -- runtime property
                       , _durability :: IntResource    -- runtime property
                       , _energy     :: IntResource    -- runtime property

                       -- , __effects :: Effects'
                       -- , __actions :: Actions' 
                       } -- Эффекты и действия не обязательно должны быть здесь. Они могут находиться и во вне, например, в списке эффектов/действий для объекта или куска карты. Буквально, на объект "навешаны" эффекты.
  deriving (Show, Read, Eq)

type Objects = [Object]

makeLenses ''Object
makeLenses ''Resource

isResourceValid (Resource c (Just m)) = (c >= 0) && (c <= m)
isResourceValid (Resource c Nothing)  = c >= 0
resourceValidator r | isResourceValid r = r
                    | otherwise         = error $ "Invalid resource property: " ++ show r
toResource (c, mbM) = resourceValidator $ Resource c mbM
