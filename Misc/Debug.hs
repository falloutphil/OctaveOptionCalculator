module Debug
   (
    tracePassThrough
   ) where

import Debug.Trace

-- Trace internal varaibles just like function
-- parameters.  May maintain your sanity.
tracePassThrough :: Show a => a -> String -> a
tracePassThrough value string
   | trace ( "Debug: " ++ string ++ " " ++ show value ) False = undefined
tracePassThrough value _ = value
