module Normal.Interface
    (
     NormalType(..),
     normalChooser
    ) where

import Normal.BoxMuller 
import Normal.Acklam   

data NormalType = NormalTypeBoxMuller BoxMuller |
                  NormalTypeAcklam    Acklam

normalChooser :: String -> NormalType
normalChooser normStr 
   | normStr == "Box Muller" = NormalTypeBoxMuller (BoxMuller Nothing)
   | normStr == "Acklam"     = NormalTypeAcklam    (Acklam ())
   | otherwise               = NormalTypeBoxMuller (BoxMuller Nothing)

