{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Barrier
   (
    DownAndOut(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.Framework
import MonteCarlo.DataStructures
import Normal.Framework


newtype DownAndOut = DownAndOut ([Bool],[Double])
   deriving (Show)

instance McClass DownAndOut where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(DownAndOut barrierStateTupleList) -> do norm <- nextNormal
                                                         let newState = DownAndOut $ evolveBarrier userData barrierStateTupleList norm
                                                         return ( () , newState )
  
   payOff (DownAndOut (barrierList,currValue)) userData = 
      [ if b then (payOffStandard (putCall userData) (strike userData) v) else 0 | v <- currValue, b <- barrierList ]

evolveBarrier :: MonteCarloUserData -> (([Bool],[Double]) -> Double -> ([Bool],[Double]))
evolveBarrier userData (barrierList,valList) normal =
   let newValue      = evolveStandard userData valList normal
       notHitBarrier = zipWith (>) newValue (barrier userData)
      in ( (zipWith (&&) notHitBarrier barrierList), newValue )
