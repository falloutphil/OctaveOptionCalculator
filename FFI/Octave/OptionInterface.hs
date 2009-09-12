 {-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}

module FFI.Octave.OptionInterface 
    where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe

import MonteCarlo.DataStructures
import Random.Interface
import Normal.Interface
import MonteCarlo.Interface
import FrameworkInterface

priceOption :: Ptr CDouble -> CSize -> CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt -> CString -> CString -> CString -> Ptr CDouble
priceOption underl underlSize strk vol expy ir ts sims rng norm instr = 

    let userData  = MonteCarloUserData { strike       = realToFrac strk,
                                         putCall      = Call,
                                         volatility   = realToFrac vol, 
                                         expiry       = realToFrac expy,
                                         interestRate = realToFrac ir,
                                         timeSteps    = fromIntegral ts }	

        numOfSims  = fromIntegral sims
        
        -- Convert C array into haskell list of doubles
        underlying = [ realToFrac value | value <- unsafePerformIO $ peekArray (fromIntegral underlSize) underl ]


        haskNorm   = unsafePerformIO $ peekCString norm
        normalType = normalChooser haskNorm
        tsteps = let ts' = timeSteps userData
                    in if even ts' then ts' else ts' + 1

        haskRng          = unsafePerformIO $ peekCString rng
        rngType          = rngChooser haskRng tsteps
        haskInstr        = unsafePerformIO $ peekCString instr
        contractType     = contractChooser haskInstr underlying
        sumOfPayOffs     = getResultFn numOfSims rngType normalType contractType $ userData
        averagePayOff    = map (*(1/(fromIntegral numOfSims))) sumOfPayOffs
        discountedPayOff = map (* exp (-1 * interestRate userData * expiry userData)) averagePayOff
        convertToCDouble :: [CDouble] = map realToFrac discountedPayOff
        convertToArray   = newArray convertToCDouble 
       in unsafePerformIO convertToArray

foreign export ccall priceOption :: Ptr CDouble -> CSize -> CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt -> CString -> CString -> CString -> Ptr CDouble






