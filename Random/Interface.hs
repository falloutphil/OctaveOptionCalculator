module Random.Interface
    (
     RngType(..),
     rngChooser
    ) where

import Data.Word (Word64)

import qualified Random.Halton 
import qualified Random.Ranq1 


-- In somewhat of a paradox we need
-- to unite our purposefully different state types
-- so that they can be passed around under a single
-- guise.  We use 'data', because 'newtype' must
-- have exactly one constructor.
data RngType = RngTypeHalton Random.Halton.Halton | 
               RngTypeRanq1  Random.Ranq1.Ranq1


-- Create a StateType containing the underlying
-- initial state of our RNG depending on user input.
-- We have to know our Halton dimensionality at initialization
-- because our state is effectively 2D (Base,Sim)
rngChooser :: String -> Int -> RngType
rngChooser rngStr totalTsDimensions
   -- Discard first 20 Haltons
   | rngStr == "Halton" = haltonInit (20,totalTsDimensions)
   | rngStr == "Ranq1"  = ranq1Init 1
   | otherwise          = haltonInit (20,totalTsDimensions)


haltonInit :: (Int, Int) -> RngType
haltonInit = RngTypeHalton . Random.Halton.initialize 

ranq1Init :: Word64 -> RngType
ranq1Init = RngTypeRanq1 . Random.Ranq1.initialize