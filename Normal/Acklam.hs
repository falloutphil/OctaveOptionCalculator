
module Normal.Acklam
   (
    Acklam(..)
   ) where

import Control.Monad.State.Strict
-- Unboxing the arrays doesn't really buy us much
-- but I consider it more correct for this 'Array' will probably
-- introduce one level of indirection (pointer to array value),
-- as all values are going to be used, boxing is pointless.
import Data.Array.Unboxed

import Random.Framework
import Normal.Framework


newtype Acklam = Acklam ()
   deriving (Show)

-- Acklam's method doesn't require any 
-- state so we are effectively creating
-- a stateless state monad transformer!
-- Compiler should (hopefully) recognise this!
instance NormalClass Acklam where
   nextNormal = StateT $ \_ -> do rn:_ <- rngStateFn 1
                                  return ( invnorm rn, Acklam () )



a, b, c, d:: UArray Int Double

a = listArray (1, 6) [-3.969683028665376e+01, 2.209460984245205e+02, 
                     -2.759285104469687e+02, 1.383577518672690e+02, 
                     -3.066479806614716e+01, 2.506628277459239e+00]

b = listArray (1, 5) [-5.447609879822406e+01, 1.615858368580409e+02, 
                     -1.556989798598866e+02, 6.680131188771972e+01, 
                     -1.328068155288572e+01]

c = listArray (1, 6) [-7.784894002430293e-03, -3.223964580411365e-01, 
                     -2.400758277161838e+00, -2.549732539343734e+00, 
                     4.374664141464968e+00,  2.938163982698783e+00]
  
d = listArray (1, 4) [7.784695709041462e-03,  3.224671290700398e-01, 
                     2.445134137142996e+00,  3.754408661907416e+00]


invnorm :: Double -> Double
invnorm p 
   | p < 0.02425 = 
      let q = sqrt ( -2*log(p) ) 
         in (((((c!1*q+c!2)*q+c!3)*q+c!4)*q+c!5)*q+c!6) / 
            ((((d!1*q+d!2)*q+d!3)*q+d!4)*q+1)
                          
                            
   | p > (1-0.02425) = 
      let q = sqrt ( -2*log(1-p) ) 
         in -(((((c!1*q+c!2)*q+c!3)*q+c!4)*q+c!5)*q+c!6) / 
            ((((d!1*q+d!2)*q+d!3)*q+d!4)*q+1)
                              

   | otherwise = 
      let q = p-0.5
          r = q*q
         in (((((a!1*r+a!2)*r+a!3)*r+a!4)*r+a!5)*r+a!6)*q / 
            (((((b!1*r+b!2)*r+b!3)*r+b!4)*r+b!5)*r+1) 


