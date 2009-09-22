{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Binary
   (
    Binary(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.DataStructures
import MonteCarlo.Framework
import Normal.Framework


newtype Binary = Binary [Double]
   deriving (Show)

instance McClass Binary where
   nextTimeStep userData = 
      StateT $ \(Binary s) -> do norm <- nextNormal
                                 let !newState = evolveClosedForm userData s norm
                                 return ( (), Binary newState )
   
   payOff (Binary values) userData = map (payOffBinary (putCall userData) (strike userData)) values

payOffBinary :: PutCall -> Double ->  Double -> Double
payOffBinary putCallB strikeB value = 
   if (payOffStandard putCallB strikeB value) > 0 then 1 else 0



