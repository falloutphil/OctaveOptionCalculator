{-# LANGUAGE BangPatterns #-}

module MonteCarlo.European
   (
    European(..)
   ) where

import Control.Monad.State.Strict

import MonteCarlo.Framework
import Normal.Framework


newtype European = European Double
   deriving (Show)

instance McClass European where
   nextTimeStep userData = 
      StateT $ \(European s) -> do norm <- nextNormal
                                   let !newState = European $ evolveClosedForm userData s norm
                                   return ( (), newState )
   toValue (European value) = value



