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


priceOption :: Ptr CDouble -> CSize -> CDouble -> CDouble -> Ptr CDouble -> CDouble -> CInt -> CInt -> CString -> CString -> CString -> CString -> Ptr CDouble
priceOption underl underlSize strk vol expy ir ts sims putOrCall rng norm instr = 
    -- Lets make out C types into Haskell equivalents
    let underlying = [ realToFrac value | value <- unsafePerformIO $ peekArray (fromIntegral underlSize) underl ]
        hsExpy     = [ realToFrac value | value <- unsafePerformIO $ peekArray (fromIntegral underlSize) expy   ]
        hsBarrier  = replicate (length underlying) 80
        numOfSims  = fromIntegral sims        
        pc =  unsafePerformIO $ peekCString putOrCall
         
        userData  = MonteCarloUserData { strike       = realToFrac strk,
                                         putCall      = read pc,
                                         volatility   = realToFrac vol, 
                                         expiry       = hsExpy,
                                         interestRate = realToFrac ir,
                                         timeSteps    = fromIntegral ts,
                                         barrier      = hsBarrier }	

        -- Halton requires an even number of dimensions
        -- to initialise - so we fudge this if the user
        -- doesn't oblige
        tsteps = let ts' = timeSteps userData
                    in if even ts' then ts' else ts' + 1

        -- Convert out C strings into custom Haskell types
        haskRng          = unsafePerformIO $ peekCString rng
        rngType          = rngChooser haskRng tsteps
        haskNorm         = unsafePerformIO $ peekCString norm
        normalType       = normalChooser haskNorm
        haskInstr        = unsafePerformIO $ peekCString instr
        contractType     = contractChooser haskInstr underlying

        -- Ask the engine for a function to calc, pass the userData to the returned func
        sumOfPayOffs     = getResultFn numOfSims rngType normalType contractType $ userData
        -- Find the average profit from the returned sums
        averagePayOff    = map (*(1/(fromIntegral numOfSims))) sumOfPayOffs
        -- List of discount factors to apply to our average returns
        discountList     = map (exp . (* (-1 * interestRate userData))) $ expiry userData
        -- Final list of PVs                   
        discountedPayOff = zipWith (*) discountList averagePayOff

        -- Put the result back in a C array
        convertToCDouble = map realToFrac discountedPayOff
        -- newArray allocates enough memory for values, 
        -- and then “pokes” the contents of the list 
        -- into that area of memory and returns a pointer.
        -- From the specs I think this is safe (i.e. not local).
        convertToArray   = newArray convertToCDouble 
       in unsafePerformIO convertToArray

foreign export ccall priceOption :: Ptr CDouble -> CSize -> CDouble -> CDouble -> Ptr CDouble -> CDouble -> CInt -> CInt -> CString -> CString -> CString -> CString -> Ptr CDouble






