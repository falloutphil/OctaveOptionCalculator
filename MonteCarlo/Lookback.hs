{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Lookback
   (
    Lookback(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.Framework
import MonteCarlo.DataStructures
import Normal.Framework


newtype Lookback = Lookback [(Double,Double)]
   deriving (Show)

instance McClass Lookback where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(Lookback maxStateTupleList) -> do norm <- nextNormal
                                                   let newState = Lookback $ evolveLookback userData maxStateTupleList norm
                                                   return ( () , newState )
   toValue (Lookback maxStateTupleList) = map fst maxStateTupleList


evolveLookback :: MonteCarloUserData -> ([(Double,Double)] -> Double -> [(Double,Double)])
evolveLookback userData maxStateTupleList normal =
   let (maxList,valList) = unzip maxStateTupleList
       newValue = evolveStandard userData valList normal
      in zip (zipWith max newValue maxList) newValue 
