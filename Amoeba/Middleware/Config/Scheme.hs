module Middleware.Config.Scheme where

import Middleware.Config.Config

deflt = sect "DEFAULT"
appName = deflt <| opt "applicationName"
logPath = deflt <| opt "logPath"
dataPath = deflt <| opt "dataPath"

video = sect "VIDEO"
screenWidth  = video <| opt "screenWidth"
screenHeight = video <| opt "screenHeight"
colorDepth   = video <| opt "colorDepth"