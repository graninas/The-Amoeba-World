module Middleware.Config.Config where

import qualified Data.ConfigFile as CF
import Data.Either.Utils

data Section = Section String
data Option = Option String

data Config = Config String String
  deriving (Show, Read, Eq)

sect = Section
opt = Option

(Section s) <| (Option o) = Config s o

getSection (Config s _) = s
getOption (Config _ o) = o

getConfig cp cfg = forceEither $ CF.get cp (getSection cfg) (getOption cfg)

loadConfiguration fileName = do
    conf <- CF.readfile CF.emptyCP fileName
    let cp = forceEither conf
    return cp {CF.optionxform = id}