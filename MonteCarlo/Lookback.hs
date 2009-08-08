{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Lookback
   (
    Lookback(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.Framework
import MonteCarlo.DataStructures
import Normal.Framework


newtype Lookback = Lookback (Double,Double)
   deriving (Show)

instance McClass Lookback where
   nextTimeStep userData = 
      StateT $ \(Lookback (maxVal,s)) -> do norm <- nextNormal
                                            let !newState = Lookback $ evolveLookback userData s norm maxVal
                                            return ( () , newState )
   toValue (Lookback (maxVal,_)) = maxVal


evolveLookback :: MonteCarloUserData -> (Double -> Double -> Double -> (Double,Double))
evolveLookback userData currentValue normal currentMaxValue =
   let newValue = evolveStandard userData currentValue normal
      in (  max newValue currentMaxValue, newValue )
