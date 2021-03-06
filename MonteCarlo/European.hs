{-# LANGUAGE BangPatterns #-}

module MonteCarlo.European
   (
    European(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.DataStructures
import MonteCarlo.Framework
import Normal.Framework


newtype European = European [Double]
   deriving (Show)

instance McClass European where
   nextTimeStep userData = 
      StateT $ \(European s) -> do norm <- nextNormal
                                   let !newState = evolveClosedForm userData s norm
                                   return ( (), European newState )
   
   payOff (European values) userData = map (payOffStandard (putCall userData) (strike userData)) values



