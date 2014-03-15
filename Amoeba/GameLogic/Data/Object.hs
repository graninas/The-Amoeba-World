{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Data.Object where

import Control.Lens

import GameLogic.Data.Player
import GameLogic.Data.Types

data Resource a = Resource { _stock :: a
                           , _capacity :: a }
  deriving (Show, Read, Eq)
  
type IntResource = Resource Int

data Object = Object {
                        -- Properties:
                         _objectId :: ObjectId          -- static property
                       , _objectType :: ObjectType      -- predefined property
                       
                       -- Runtime properties, resources:
                       , _ownership :: Player           -- runtime property... or can be effect!

                       , _lifebound  :: IntResource    -- runtime property
                       , _durability :: IntResource    -- runtime property
                       , _energy     :: IntResource    -- runtime property

                       -- , __effects :: Effects'
                       -- , __actions :: Actions' 
                       }
  deriving (Show, Read, Eq)

type Objects = [Object]

makeLenses ''Object
makeLenses ''Resource

isResourceValid (s, c) = (s >= 0) && (s <= c || c == 0)
toResource = uncurry Resource

