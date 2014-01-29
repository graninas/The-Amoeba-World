module Middleware.Config.Config where

import qualified Data.ConfigFile as CF
import Data.Either.Utils

import qualified Control.Monad.Reader as R

data Sect = Sect String
data Opt = Opt String
data Cfg = Cfg String String

sect = Sect
opt = Opt

(Sect s) <| (Opt o) = Cfg s o

getSect (Cfg s _) = s
getOpt (Cfg _ o) = o

getOption cp cfg = forceEither $ CF.get cp (getSect cfg) (getOpt cfg)

loadConfiguration fileName = do
    conf <- CF.readfile CF.emptyCP fileName
    let cp = forceEither conf
    return cp {CF.optionxform = id}

intOption :: Cfg -> R.Reader CF.ConfigParser Int
intOption cfg = do
    cp <- R.ask
    return $ getOption cp cfg
    
stringOption :: Cfg -> R.Reader CF.ConfigParser String
stringOption cfg = do
    cp <- R.ask
    return $ getOption cp cfg
    
getConfig cp loader = return $ R.runReader loader cp

