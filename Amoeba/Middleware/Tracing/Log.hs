module Middleware.Tracing.Log
    ( setupLogger
    , finish
    , info
    , warning
    , debug
    , error
    , notice ) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Prelude hiding (error)

import Paths_The_Amoeba_World as P

logFormat = "$utcTime $prio $loggername: $msg"
loggerName = "Amoeba.Application"

defaultFormatter = simpleLogFormatter logFormat

setupLogger logFileName = do
    logFileRealName <- P.getDataFileName logFileName
    handler <- fileHandler logFileRealName INFO
    let handler' = setFormatter handler defaultFormatter
    updateGlobalLogger loggerName $ addHandler handler'
    updateGlobalLogger loggerName $ setLevel INFO
    
finish = removeAllHandlers

info = infoM loggerName
warning = warningM loggerName
debug = debugM loggerName
error = errorM loggerName
notice = noticeM loggerName
