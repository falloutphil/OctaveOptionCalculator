module Maths.Prime
   (
     primes
   ) where


-- This method is very simple and OK for n<1000.  
-- If you're using Halton >12D, you're not too smart, so 1000 is more than fine!
primes :: [Int]
primes = sieve [2..]
            where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]
                  sieve _      = error "sieve called with empty list"