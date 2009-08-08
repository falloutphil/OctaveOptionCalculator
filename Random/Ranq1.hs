{-# LANGUAGE BangPatterns #-}

module Random.Ranq1
   (
    Ranq1,
    initialize
   ) where

import Control.Monad.State.Strict
import Data.Word (Word64)
import Data.Bits (shift,xor)

import Random.Framework


newtype Ranq1 = Ranq1 Word64
   deriving(Show)

                                               
instance RngClass Ranq1 where
   rngStateFn dims = State $ \(Ranq1 s) -> let (vector,!state) = multiRanq1 ([],s) dims
                                              in (vector, Ranq1 state)


initialize :: Word64 -> Ranq1
initialize = Ranq1 . convertToWord64 . 
                     ranq1Increment  . 
                     ( xor 4101842887655102017 )


-- Reversing isn't ideal - but it is better than appending to the end each time.
-- The list is only rebuilt once with a reverse and will be O(Dimensions).
-- Not expecting a huge amount of dimensions, so this will suffice for now.
-- http://www.haskell.org/pipermail/beginners/2009-February/000882.html
multiRanq1 :: ([Double],Word64) -> Int -> ([Double], Word64)
multiRanq1 (vector,state) dims 
   | dims <= 0 = (reverse vector,state)
   | otherwise = multiRanq1 (newVector,newState) (dims-1)
                    where
                       newVector = convertToDouble state : vector
                       newState  = ranq1Increment state 


convertToWord64 :: Word64 -> Word64
convertToWord64 = (*2685821657736338717)

convertToDouble :: Word64 -> Double
convertToDouble = (*5.42101086242752217E-20) . 
                  fromIntegral               . 
                  convertToWord64
  
ranq1Increment :: Word64 -> Word64
ranq1Increment =  ( `ranq1XorShift` (-4) ) . 
                  ( `ranq1XorShift` 35 )   . 
                  ( `ranq1XorShift` (-21) )   

ranq1XorShift :: Word64 -> Int -> Word64
ranq1XorShift v = (xor v) . (shift v)
 
