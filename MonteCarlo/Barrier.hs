{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Barrier
   (
    DownAndOut(..),
    DownAndIn(..),
    UpAndOut(..),
    UpAndIn(..)
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
                                                         let newState = DownAndOut $ evolveDown userData barrierStateTupleList norm
                                                         return ( () , newState )
  
   payOff (DownAndOut (barrierList,currValue)) userData = 
      [ if b then (payOffStandard (putCall userData) (strike userData) v) else 0 | (v,b) <- zip currValue barrierList ]




newtype DownAndIn = DownAndIn ([Bool],[Double])
   deriving (Show)

instance McClass DownAndIn where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(DownAndIn barrierStateTupleList) -> do norm <- nextNormal
                                                        let newState = DownAndIn $ evolveDown userData barrierStateTupleList norm
                                                        return ( () , newState )
  
   payOff (DownAndIn (barrierList,currValue)) userData = 
      [ if b then 0 else (payOffStandard (putCall userData) (strike userData) v) | (v,b) <- zip currValue barrierList ]



newtype UpAndOut = UpAndOut ([Bool],[Double])
   deriving (Show)

instance McClass UpAndOut where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(UpAndOut barrierStateTupleList) -> do norm <- nextNormal
                                                       let newState = UpAndOut $ evolveUp userData barrierStateTupleList norm
                                                       return ( () , newState )
  
   payOff (UpAndOut (barrierList,currValue)) userData = 
      [ if b then (payOffStandard (putCall userData) (strike userData) v) else 0 | (v,b) <- zip currValue barrierList ]




newtype UpAndIn = UpAndIn ([Bool],[Double])
   deriving (Show)

instance McClass UpAndIn where
   nextTimeStep userData =
      -- NOTE: Strict on the constituents of the tuple to prevent thunks 
      StateT $ \(UpAndIn barrierStateTupleList) -> do norm <- nextNormal
                                                      let newState = UpAndIn $ evolveUp userData barrierStateTupleList norm
                                                      return ( () , newState )
  
   payOff (UpAndIn (barrierList,currValue)) userData = 
      [ if b then 0 else (payOffStandard (putCall userData) (strike userData) v) | (v,b) <- zip currValue barrierList ]



evolveDown :: MonteCarloUserData -> (([Bool],[Double]) -> Double -> ([Bool],[Double]))
evolveDown = evolveBarrier (>)

evolveUp :: MonteCarloUserData -> (([Bool],[Double]) -> Double -> ([Bool],[Double]))
evolveUp = evolveBarrier (<)

evolveBarrier :: (Double -> Double -> Bool) -> MonteCarloUserData -> (([Bool],[Double]) -> Double -> ([Bool],[Double]))
evolveBarrier inEq userData (barrierList,valList) normal =
   let newValue      = evolveStandard userData valList normal
       notHitBarrier = zipWith (inEq) newValue (barrier userData)
      in ( (zipWith (&&) notHitBarrier barrierList), newValue )

