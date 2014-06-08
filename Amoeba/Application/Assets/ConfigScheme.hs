module Application.Assets.ConfigScheme where

import Middleware.Config.Config

defaultSection = sect "DEFAULT"
appName  = defaultSection <| opt "applicationName"
rootPath = defaultSection <| opt "rootPath"
logPath  = defaultSection <| opt "logPath"
dataPath = defaultSection <| opt "dataPath"
rawsPath = defaultSection <| opt "rawsPath"


videoSection = sect "VIDEO"
screenWidth = videoSection <| opt "screenWidth"
screenHeight = videoSection <| opt "screenHeight"
colorDepth = videoSection <| opt "colorDepth"
virtualPlaneX = videoSection <| opt "virtualPlaneX"
virtualPlaneY = videoSection <| opt "virtualPlaneY"