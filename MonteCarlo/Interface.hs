module MonteCarlo.Interface
    (
     ContractType(..),
     contractChooser
    ) where

import MonteCarlo.European 
import MonteCarlo.Lookback
import MonteCarlo.Asian

data ContractType = ContractTypeEuropean      European    |
                    ContractTypeLookback      Lookback    |
                    ContractTypeAsianAvRate   AsianAvRate |
                    ContractTypeAsianAvStrike AsianAvStrike


contractChooser :: String -> [Double] -> ContractType
contractChooser contractStr initialValue 
   | contractStr == "European" = ContractTypeEuropean      (European initialValue)
   | contractStr == "Lookback" = ContractTypeLookback      (Lookback      $ ((repeat 0), initialValue))
   | contractStr == "ARO"      = ContractTypeAsianAvRate   (AsianAvRate   $ ((repeat 0), initialValue))
   | contractStr == "ASO"      = ContractTypeAsianAvStrike (AsianAvStrike $ ((repeat 0), initialValue))
   | otherwise                 = ContractTypeEuropean      (European initialValue)



