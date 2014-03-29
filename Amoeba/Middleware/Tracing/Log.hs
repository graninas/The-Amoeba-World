module Middleware.Tracing.Log (setupLogger, debug) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

logFormat = "$utcTime $prio $loggername: $msg"
loggerName = "Amoeba.Application"

defaultFormatter = simpleLogFormatter logFormat

setupLogger logFileName = do
    handler <- fileHandler logFileName NOTICE
    let handler' = setFormatter handler defaultFormatter
    updateGlobalLogger loggerName $ addHandler handler'
    
debug = debugM loggerName