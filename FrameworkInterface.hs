
module FrameworkInterface
    (
     getResultFn
    ) where

import Random.Interface
import Normal.Interface
import MonteCarlo.Interface
import MonteCarlo.DataStructures

import Normal.Framework
import MonteCarlo.Framework

-- Yuk, the last bit of boilerplate!
-- Returns a function takes the user data 
-- and produces a result.
getResultFn :: Int -> RngType -> NormalType -> ContractType 
               -> ( MonteCarloUserData -> [Double] )
getResultFn numOfSims rng norm (ContractTypeEuropean euro)     = 
   getNormalAndRngFn numOfSims rng norm $ euro
getResultFn numOfSims rng norm (ContractTypeLookback lb)       = 
  getNormalAndRngFn numOfSims rng norm $ lb
getResultFn numOfSims rng norm (ContractTypeAsianAvRate rateAsian)  = 
  getNormalAndRngFn numOfSims rng norm $ rateAsian
getResultFn numOfSims rng norm (ContractTypeAsianAvStrike strikeAsian) = 
  getNormalAndRngFn numOfSims rng norm $ strikeAsian
getResultFn numOfSims rng norm (ContractTypeBinary binary) = 
  getNormalAndRngFn numOfSims rng norm $ binary
getResultFn numOfSims rng norm (ContractTypeDownAndOut dao) = 
  getNormalAndRngFn numOfSims rng norm $ dao



getNormalAndRngFn :: McClass a =>
                     Int -> RngType -> NormalType -> 
                     ( a -> MonteCarloUserData -> [Double])
getNormalAndRngFn numOfSims rng (NormalTypeBoxMuller bm) = 
   getRngFn numOfSims rng $ bm  
getNormalAndRngFn numOfSims rng (NormalTypeAcklam ack)   = 
   getRngFn numOfSims rng $ ack 

-- Separating the decision across two functionals
-- reduces the amount of boilerplate.
-- Consider if we have 3 rngs and 3 normal generators
-- then under one functional we would have 3x3=9 combinations.
-- This way we only specify each type once, we have 3+3=6 combinations.
-- Another way to think of is that if we added a new Rng we would only
-- have to update the below function with 1 line.  If it was done
-- in one function we would have a case for each NormalType.
getRngFn :: NormalClass a => McClass b => Show a =>
            Int -> RngType -> ( a -> b -> MonteCarloUserData -> [Double])
getRngFn numOfSims (RngTypeHalton halton) = simResult numOfSims halton 
getRngFn numOfSims (RngTypeRanq1  ranq1)  = simResult numOfSims ranq1  



