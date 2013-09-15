{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Monoid
import Data.Maybe
import Control.Lens
import qualified Data.Map as Map
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.All

type PropertyMap = Map.Map Int String
data Properties = Properties { _propertyMap :: PropertyMap }
  deriving (Eq, Show)

emptyProperties = Properties Map.empty

makeLenses ''Properties

newtype PAccessor = PAccessor { pKey :: Int }

property1 = PAccessor 1
property2 = PAccessor 2
property3 = PAccessor 3

(|=) accessor v = do
    ps <- get
    put $ propertyMap . at (pKey accessor) ?~ v $ ps

setProperty = (|=)

getProperty accessor = do
    ps <- get
    return $ ps ^. propertyMap . ix (pKey accessor)
    
maybeProperty accessor = do
    ps <- get
    return $ ps ^. propertyMap . at (pKey accessor)
    
obj1Properties :: State Properties String
obj1Properties = do
    property1 |= "Property1 value"
    property2 |= "Property2 value"
    p1Val <- getProperty property1
    p3Val <- getProperty property3 -- Returns default value (empty string)
    return (p1Val ++ p3Val)

obj2Properties :: State Properties String
obj2Properties = do
    property2 |= "Property2 value"
    property3 |= "Property3 value"
    Nothing <- maybeProperty property1
    Just p2Val <- maybeProperty property2
    return p2Val

expectedProps1 = Properties $ Map.fromList [ (1, "Property1 value")
                                           , (2, "Property2 value") ]
                                      
expectedProps2 = Properties $ Map.fromList [ (2, "Property2 value")
                                           , (3, "Property3 value") ]

prop_test1 = (props == expectedProps1) && (val == "Property1 value")
    where
        (val, props) = runState obj1Properties emptyProperties
        
prop_test2 = (props == expectedProps2) && (val == "Property2 value")
    where
        (val, props) = runState obj2Properties emptyProperties

prop_test3 = val == "Property2 value"
    where
        val = evalState obj2Properties emptyProperties

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    
    putStrLn "\nObj1Properties"
    let (val, s) = runState obj1Properties emptyProperties
    print val
    print s
    
    putStrLn "\nObj2Properties"
    let (val, s) = runState obj2Properties emptyProperties
    print val
    print s