{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import System.Random
import qualified Data.List as L

import Test.Dummy
import Test.Arbitraries
import Test.Data

import World.World
import World.Geometry
import qualified World.Player as P
import qualified World.Items.Karyon as K

