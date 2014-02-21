module Main where

import Control.Monad.Error

action1 :: Either String Int
action1 = return 10

action2 :: Either String String
action2 = return "String"

callEitherMonad :: Either String String
callEitherMonad = do
    res1 <- action1
    res2 <- action2
    return $ show res1 ++ res2


main = do
    let res = callEitherMonad
    print res