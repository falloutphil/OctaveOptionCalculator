{-# LANGUAGE BangPatterns #-}

module MonteCarlo.Framework
    (
     McClass,
     nextTimeStep,
     toValue,
     simResult,
     evolveStandard,
     evolveClosedForm
    ) where

import Control.Monad.State.Strict

import Normal.Framework
import Random.Framework
import MonteCarlo.DataStructures

class McClass a where
  nextTimeStep :: NormalClass b => RngClass c =>
                  MonteCarloUserData -> StateT a (StateT b (State c)) ()
  toValue :: a -> Double

-- Here's the polymorphism!  Evaluate a monad stack
-- where the inner monad is of RngClass and outer is of NormalClass
singleResult :: RngClass a => NormalClass b => McClass c => -- Show a =>
                a -> b -> c -> MonteCarloUserData -> ( Double, (a,b) )
--singleResult initRngState initNormState userData 
--   | trace ( "   initRngState " ++ show initRngState ) False=undefined 
singleResult initRngState initNormState initMcState userData = 
   let (((_,mc_s), norm_s), rng_s) = runState rngMonad initRngState
         where rngMonad     = runStateT normMonadT initNormState
               mcCombinator = do replicateM_ (timeSteps userData) (nextTimeStep userData)
               normMonadT   = runStateT mcCombinator initMcState
      in (toValue mc_s, (rng_s,norm_s))
                                                     

simResult :: RngClass a => NormalClass b => McClass c => Show a => Show b =>
             Int -> Double -> a -> b -> c -> MonteCarloUserData -> Double
--simResult numOfSims runTotal initRng initNorm userData  
--   | trace ( "numOfSims " ++ show numOfSims ++ "   initRng " ++ show initRng ) False=undefined 
simResult numOfSims runTotal initRng initNorm initMc userData 
   | numOfSims <= 1 = runTotal
   | otherwise = let (!value,(!rngS,!normS)) = singleResult initRng initNorm initMc userData
                     !newNumOfSims           = numOfSims - 1
                     profit                  = payOff (strike userData) value (putCall userData)
                     !newRunTotal            = runTotal + profit
                    in simResult newNumOfSims newRunTotal rngS normS initMc userData


putCallMult :: Num a => PutCall -> a
putCallMult Call = 1
putCallMult Put  = -1

payOff :: Double -> Double -> PutCall -> Double
payOff strikeVal stock putcall =
   max 0 $ (putCallMult putcall)*(stock - strikeVal) 

evolveStandard :: MonteCarloUserData -> (Double -> Double -> Double)
--evolveStandard userData currentValue normal
--   | trace ( "   currentValue " ++ show currentValue ++ " normal " ++ show normal ) False=undefined 
evolveStandard userData currentValue normal = 
   let vol        = volatility userData
       delta_t    = expiry userData / fromIntegral (timeSteps userData)
       stochastic = vol * normal * sqrt delta_t
       drift      = (interestRate userData) * delta_t
      in currentValue * ( 1 + drift + stochastic)

evolveClosedForm :: MonteCarloUserData -> (Double -> Double -> Double)
--evolveClosedForm userData currentValue normal
--   | trace ( "   currentValue " ++ show currentValue ++ " normal " ++ show normal ) False=undefined 
evolveClosedForm userData currentValue normal = 
   let vol        = volatility userData
       delta_t    = expiry userData / fromIntegral (timeSteps userData)
       stochastic = vol * normal * sqrt delta_t
       drift      = ( (interestRate userData) - (0.5*vol*vol) )*delta_t
      in currentValue * exp ( drift + stochastic )

