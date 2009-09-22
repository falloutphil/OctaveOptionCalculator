module MonteCarlo.Interface
    (
     ContractType(..),
     contractChooser
    ) where

import MonteCarlo.European 
import MonteCarlo.Lookback
import MonteCarlo.Asian
import MonteCarlo.Binary
import MonteCarlo.Barrier

data ContractType = ContractTypeEuropean      European      |
                    ContractTypeLookback      Lookback      |
                    ContractTypeAsianAvRate   AsianAvRate   |
                    ContractTypeAsianAvStrike AsianAvStrike |
                    ContractTypeBinary        Binary        |
                    ContractTypeDownAndOut    DownAndOut


contractChooser :: String -> [Double] -> ContractType
contractChooser contractStr initialValue 
   | contractStr == "European"   = ContractTypeEuropean      (European initialValue)
   | contractStr == "Lookback"   = ContractTypeLookback      (Lookback      $ ((repeat 0), initialValue))
   | contractStr == "ARO"        = ContractTypeAsianAvRate   (AsianAvRate   $ ((repeat 0), initialValue))
   | contractStr == "ASO"        = ContractTypeAsianAvStrike (AsianAvStrike $ ((repeat 0), initialValue))
   | contractStr == "Binary"     = ContractTypeBinary        (Binary initialValue)
   | contractStr == "DownAndOut" = ContractTypeDownAndOut    (DownAndOut    $ ((repeat True), initialValue))
   | otherwise                   = ContractTypeEuropean      (European initialValue)



