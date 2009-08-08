module MonteCarlo.DataStructures
    (
     MonteCarloUserData(..),
     PutCall(..)
    ) where


data PutCall = Put | Call
               deriving (Read)

-- Holds only constant data
data MonteCarloUserData = MonteCarloUserData 
   { strike       :: Double,
     putCall      :: PutCall,
     volatility   :: Double,
     expiry       :: Double,
     interestRate :: Double,
     timeSteps    :: Int  }
