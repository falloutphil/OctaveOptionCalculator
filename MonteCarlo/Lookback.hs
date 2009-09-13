{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Lookback
   (
    Lookback(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.Framework
import MonteCarlo.DataStructures
import Normal.Framework


newtype Lookback = Lookback ([Double],[Double])
   deriving (Show)

instance McClass Lookback where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(Lookback maxStateTupleList) -> do norm <- nextNormal
                                                   let newState = Lookback $ evolveLookback userData maxStateTupleList norm
                                                   return ( () , newState )
   toValue (Lookback (maxList,_)) = maxList


evolveLookback :: MonteCarloUserData -> (([Double],[Double]) -> Double -> ([Double],[Double]))
evolveLookback userData (maxList,valList) normal =
   let newValue = evolveStandard userData valList normal
      in ( (zipWith max newValue maxList), newValue )
