module Random.Framework
    (
     RngClass,
     rngStateFn
    ) where

import Control.Monad.State.Strict

-- typeclass insists we produce a functional
-- that returns a State monad which returns
-- a list of doubles (n-dimensional random vector)
class RngClass a where
  rngStateFn :: Int -> State a [Double]



