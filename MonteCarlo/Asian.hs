{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Asian
   (
    AsianAvRate(..),
    AsianAvStrike(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.Framework
import MonteCarlo.DataStructures
import Normal.Framework


newtype AsianAvRate = AsianAvRate ([Double],[Double])
   deriving (Show)

instance McClass AsianAvRate where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(AsianAvRate avrStateTupleList) -> do norm <- nextNormal
                                                      let newState = AsianAvRate $ evolveAvAsian userData avrStateTupleList norm
                                                      return ( () , newState )

   -- Remember max(A-E,0) not AVERAGE(max((S-E),0))
   payOff  (AsianAvRate (summedList,_)) userData = 
      let averageFromSumFn = ( / (fromIntegral(timeSteps userData)) ) in 
         map ( (payOffStandard (putCall userData) (strike userData)) . averageFromSumFn ) summedList 



newtype AsianAvStrike = AsianAvStrike ([Double],[Double])
   deriving (Show)

instance McClass AsianAvStrike where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(AsianAvStrike avrStateTupleList) -> do norm <- nextNormal
                                                        let newState = AsianAvStrike $ evolveAvAsian userData avrStateTupleList norm
                                                        return ( () , newState )

 
   payOff  (AsianAvStrike (summedList,finalValues)) userData = 
      let strikes = map ( / (fromIntegral(timeSteps userData)) ) summedList in
         zipWith (payOffStandard (putCall userData)) strikes finalValues



evolveAvAsian :: MonteCarloUserData -> (([Double],[Double]) -> Double -> ([Double],[Double]))
evolveAvAsian userData (summedList,valList) normal =
   let newValue = evolveStandard userData valList normal
      in ( zipWith (+) newValue summedList, newValue )

