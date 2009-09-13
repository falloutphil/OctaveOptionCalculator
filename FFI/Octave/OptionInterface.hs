{-# LANGUAGE ForeignFunctionInterface#-}

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

import Misc.Debug

priceOption :: Ptr CDouble -> CSize -> CDouble -> CDouble -> Ptr CDouble -> CDouble -> CInt -> CInt -> CString -> CString -> CString -> CString -> Ptr CDouble
priceOption underl underlSize strk vol expy ir ts sims putOrCall rng norm instr = 

    let underlying = [ realToFrac value | value <- unsafePerformIO $ peekArray (fromIntegral underlSize) underl ]
        hsExpy     = [ realToFrac value | value <- unsafePerformIO $ peekArray (fromIntegral underlSize) expy ]
        pc =  tracePassThrough (unsafePerformIO $ peekCString putOrCall) "pc"
        userData  = MonteCarloUserData { strike       = realToFrac strk,
                                         putCall      = read pc  :: PutCall,
                                         volatility   = realToFrac vol, 
                                         expiry       = hsExpy,
                                         interestRate = realToFrac ir,
                                         timeSteps    = fromIntegral ts }	

        numOfSims  = fromIntegral sims        
        haskNorm   = tracePassThrough (unsafePerformIO $ peekCString norm) "norm"
        normalType = normalChooser haskNorm
        tsteps = let ts' = timeSteps userData
                    in if even ts' then ts' else ts' + 1

        haskRng          = tracePassThrough (unsafePerformIO $ peekCString rng) "rng"
        rngType          = rngChooser haskRng tsteps
        haskInstr        = tracePassThrough (unsafePerformIO $ peekCString instr) "instr"
        contractType     = contractChooser haskInstr underlying
        sumOfPayOffs     = getResultFn numOfSims rngType normalType contractType $ userData
        averagePayOff    = map (*(1/(fromIntegral numOfSims))) sumOfPayOffs
        discountListCf   = map (* (-1 * interestRate userData)) $ expiry userData
        discountList     = map exp discountListCf
        discountedPayOff = zipWith (*) discountList averagePayOff
        convertToCDouble = map realToFrac discountedPayOff
        convertToArray   = newArray convertToCDouble 
       in unsafePerformIO convertToArray

foreign export ccall priceOption :: Ptr CDouble -> CSize -> CDouble -> CDouble -> Ptr CDouble -> CDouble -> CInt -> CInt -> CString -> CString -> CString -> CString -> Ptr CDouble






