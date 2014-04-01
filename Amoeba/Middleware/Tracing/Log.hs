module Middleware.Tracing.Log (setupLogger, info, finish) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

logFormat = "$utcTime $prio $loggername: $msg"
loggerName = "Amoeba.Application"

defaultFormatter = simpleLogFormatter logFormat

setupLogger logFileName = do
    handler <- fileHandler logFileName INFO
    let handler' = setFormatter handler defaultFormatter
    updateGlobalLogger loggerName $ addHandler handler'
    updateGlobalLogger loggerName $ setLevel INFO
    
info = infoM loggerName

finish = removeAllHandlers