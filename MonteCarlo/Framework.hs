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
  toValue :: a -> [Double]

-- Evaluate a child monad stack
-- Represent a single evolution of an underlying
-- from t=0 to t=T
singleResult :: RngClass a => NormalClass b => McClass c => -- Show a =>
                a -> b -> c -> MonteCarloUserData -> ( [Double], (a,b) )
--singleResult initRngState initNormState userData 
--   | trace ( "   initRngState " ++ show initRngState ) False=undefined 
singleResult initRngState initNormState initMcState userData = 
   let (((_,mc_s), norm_s), rng_s) = runState rngMonad initRngState
         where rngMonad     = runStateT normMonadT initNormState
               mcCombinator = do replicateM_ (timeSteps userData) (nextTimeStep userData)
               normMonadT   = runStateT mcCombinator initMcState
      in (toValue mc_s, (rng_s,norm_s))
                                                     

-- Daddy monad.  Keeps a running sum of the profit
-- generated by the child monad stack and passes state
-- data between adjacent stock evolutions
simState :: RngClass a => NormalClass b =>  McClass c => 
            MonteCarloUserData -> c -> State ([Double],(a,b)) ()
simState userData initUnderl = 
   State $ \(curSum,(rng_s,norm_s)) -> let (value,states) = singleResult rng_s norm_s initUnderl userData
                                           profit         = map (payOff (strike userData) (putCall userData)) value
                                           !newSum        = zipWith (+) profit curSum
                                          in ((),(newSum,states))
                                  

-- Daddy function. Entry point to our engine - executes the Daddy monad
simResult :: RngClass a => NormalClass b => McClass c => Show a => Show b =>
             Int -> a -> b -> c -> MonteCarloUserData -> [Double]
simResult numOfSims initRng initNorm initMc userData = 
   let (finalSum,_) = execState (do replicateM_ numOfSims (simState userData initMc)) (repeat 0,(initRng,initNorm))
      in finalSum


putCallMult :: Num a => PutCall -> a
putCallMult Call = 1
putCallMult Put  = -1

payOff :: Double -> PutCall -> Double -> Double
payOff strikeVal putcall stock =
   max 0 $ (putCallMult putcall)*(stock - strikeVal) 

evolveStandard :: MonteCarloUserData -> ([Double] -> Double -> [Double])
--evolveStandard userData currentValue normal
--   | trace ( "   currentValue " ++ show currentValue ++ " normal " ++ show normal ) False=undefined 
evolveStandard userData currentValue normal = 
   let vol        = volatility userData
       delta_t    = expiry userData / fromIntegral (timeSteps userData)
       stochastic = vol * normal * sqrt delta_t
       drift      = (interestRate userData) * delta_t
      in map (* ( 1 + drift + stochastic)) currentValue 

evolveClosedForm :: MonteCarloUserData -> ([Double] -> Double -> [Double])
--evolveClosedForm userData currentValue normal
--   | trace ( "   currentValue " ++ show currentValue ++ " normal " ++ show normal ) False=undefined 
evolveClosedForm userData currentValue normal = 
   let vol        = volatility userData
       delta_t    = expiry userData / fromIntegral (timeSteps userData)
       stochastic = vol * normal * sqrt delta_t
       drift      = ( (interestRate userData) - (0.5*vol*vol) )*delta_t
      in map (* exp ( drift + stochastic )) currentValue

