
module Normal.BoxMuller
   (
    BoxMuller(..)
   ) where

import Control.Monad.State.Strict

import Random.Framework
import Normal.Framework


newtype BoxMuller = BoxMuller (Maybe Double)
   deriving (Show)

instance NormalClass BoxMuller where
   nextNormal = 
      StateT $ \(BoxMuller s) -> case s of
                                    Just d  -> return (d,BoxMuller Nothing)
	                            Nothing -> do rn1:rn2:_ <- rngStateFn 2
	                                          let (norm1,norm2) = boxMuller rn1 rn2
   				                  return (norm1,BoxMuller (Just norm2))


boxMuller :: Double -> Double -> (Double,Double)
boxMuller rn1 rn2 = (normal1,normal2)
  where
    r        = sqrt ( (-2)*log rn1)
    twoPiRn2 = 2 * pi * rn2
    normal1  = r * cos ( twoPiRn2 )
    normal2  = r * sin ( twoPiRn2 )


