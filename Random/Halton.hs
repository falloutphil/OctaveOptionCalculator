{-# LANGUAGE BangPatterns #-}

module Random.Halton
   (
    Halton,
    initialize
   ) where

import Control.Monad.State.Strict

import Random.Framework
import Maths.Prime

-- The underlying state of a halton sequence
-- is just an int, but if we want to have
-- many instances of RngClass each with
-- an underlying state of Int, then
-- we must differentiate.  We need a
-- a *new* type exclusive of, but synonmous
-- with an int.  We reduce this to an Int
-- once we've drilled through the polymorphism.
-- deriving(Show) needed to use with trace function.
newtype Halton = Halton [Double]
  deriving(Show)

-- The complex function below makes this a doddle.
-- Note we NEVER force evaluation of the infinite list!
instance RngClass Halton where
  rngStateFn dims =
     State $ \(Halton s) -> let (!returned,rest) = splitAt dims s in (returned,Halton rest)


-- Wrapped in RngType so 'Halton' state
-- can be thread through the getResultFn
initialize :: (Int,Int) -> Halton
initialize (initialState,totalDims) = 
   -- Infinite list of primes cycled according
   -- to our dimensionality is zipped with
   -- the corresponding simulation number.
   -- Unlike PRNG we actually define the entire
   -- series before we even start simulating,
   -- such is the joy of Haskell.
   -- Our state monad just takes the next item
   -- in the list we generate and returns the rest.
   -- Can't decide if this is very clever or
   -- somewhat obfuscated :-)
   let primeList  = cycle $ take totalDims primes
       simNumList = [ tsSeeds | seeds <- [initialState..], tsSeeds <- replicate totalDims seeds ]
      in Halton [ reflect (sim,1,0) prime | (sim,prime) <- zip simNumList primeList ]

type ReflectionState = (Int,Double,Double)

reflect :: ReflectionState -> Int -> Double
reflect (k,f,h) base
   | k <= 0 = h
   | otherwise = reflect (newK,newF,newH) base
       where
         newK = k `div` base
         newF = f / fromIntegral base
         newH = h + fromIntegral(k `mod` base) * newF


