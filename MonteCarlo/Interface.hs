module MonteCarlo.Interface
    (
     ContractType(..),
     contractChooser
    ) where

import MonteCarlo.European 
import MonteCarlo.Lookback

data ContractType = ContractTypeEuropean European |
                    ContractTypeLookback Lookback


contractChooser :: String -> [Double] -> ContractType
contractChooser contractStr initialValue 
   | contractStr == "European" = ContractTypeEuropean (European initialValue)
   | contractStr == "Lookback" = ContractTypeLookback (Lookback $ zip (repeat 0) initialValue)
   | otherwise                 = ContractTypeEuropean (European initialValue)



