module Normal.Framework
    (
     NormalClass,
     nextNormal
    ) where

import Control.Monad.State.Strict

import Random.Framework

class NormalClass a where
  nextNormal :: RngClass b => StateT a (State b) Double

