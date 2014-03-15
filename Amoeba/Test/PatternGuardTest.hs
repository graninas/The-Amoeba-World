module Main where

import Control.Monad.State

baz = return 200

foo :: Int -> String
foo n | evalState (do   m <- baz
                        return (n == m - 190)) ()
                    = "N == 10!!!"
foo _ = "N != 10!!!"

main :: IO ()
main = do
    print $ foo 15
    print $ foo 10