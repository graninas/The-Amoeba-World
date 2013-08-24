module Main where

import Application.Boot

main::IO ()
main = do

    putStrLn "Loading..."
    
    boot
    
    putStrLn $ "All Ok."