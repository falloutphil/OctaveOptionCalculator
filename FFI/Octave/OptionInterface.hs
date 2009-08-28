 {-# LANGUAGE ForeignFunctionInterface#-}

module FFI.Octave.OptionInterface 
    where

import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

import MonteCarlo.DataStructures
import Random.Interface
import Normal.Interface
import MonteCarlo.Interface
import FrameworkInterface

priceOption :: CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble --CString -> CString -> CString -> CDouble
priceOption strk vol expy ir ts sims = -- rng norm instr = 

    let userData  = MonteCarloUserData { strike       = realToFrac strk,
                                         putCall      = Call,
                                         volatility   = realToFrac vol, 
                                         expiry       = realToFrac expy,
                                         interestRate = realToFrac ir,
                                         timeSteps    = fromIntegral ts }	

        numOfSims  = fromIntegral sims
        underlying = 100
        haskNorm   = "Box Muller" --unsafePerformIO $ peekCString norm
        normalType = normalChooser haskNorm
        tsteps = let ts' = timeSteps userData
                    in if even ts' then ts' else ts' + 1

        haskRng          = "Halton" --unsafePerformIO $ peekCString rng
        rngType          = rngChooser haskRng tsteps
        haskInstr        = "European" --unsafePerformIO $ peekCString instr
        contractType     = contractChooser haskInstr underlying
        sumOfPayOffs     = getResultFn numOfSims rngType normalType contractType $ userData
        averagePayOff    = sumOfPayOffs / fromIntegral numOfSims
        discountedPayOff = averagePayOff * exp (-1 * interestRate userData * expiry userData)
       in realToFrac discountedPayOff

foreign export ccall priceOption :: CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble --CString -> CString -> CString -> CDouble






