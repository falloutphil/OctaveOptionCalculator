module Misc.Debug
   (
    tracePassThrough
   ) where

import Debug.Trace

-- Trace internal variables just like function
-- parameters.  May maintain your sanity.
tracePassThrough :: Show a => a -> String -> a
tracePassThrough value string
   | trace ( "Debug: " ++ string ++ " " ++ show value ) False = undefined
tracePassThrough value _ = value
