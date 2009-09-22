

import Random.Interface
import Normal.Interface
import MonteCarlo.Interface
import MonteCarlo.DataStructures

import FrameworkInterface


main :: IO()
main = do

          -- BS = 5.191
          let userData = MonteCarloUserData { strike       = 100, --52, --100, 
                                              putCall      = Call,
                                              volatility   = 0.2, --0.4, --0.2,  
                                              expiry       = [1,1], --5/12, --1, 
                                              interestRate = 0.05, --0.1, --0.05,
                                              timeSteps    = 1,
                                              barrier      = [90,90]}                      
              numOfSims = 500000
              userRng = "Ranq1"
              userNorm = "Acklam"
              userContract = "European"
              underLying = [100,100]
              normalType = normalChooser userNorm
              ts = let ts' = timeSteps userData
                      in if even ts' then ts' else ts' + 1
              rngType          = rngChooser userRng ts
              contractType     = contractChooser userContract underLying
              sumOfPayOffs     = getResultFn numOfSims rngType normalType contractType $ userData
              averagePayOff    = map (*(1/(fromIntegral numOfSims))) sumOfPayOffs
              discountListCf   = map (* (-1 * interestRate userData)) $ expiry userData
              discountList     = map exp discountListCf
              discountedPayOff = zipWith (*) discountList averagePayOff
          putStrLn "Result:"
          print discountedPayOff














 {-putStrLn "Random Number Generator?"
          userRng <- getLine
          putStrLn "Normal Generator?"
          userNorm <- getLine
          putStrLn "Strike?"
          userStrike <- getLine
          putStrLn "Underlying Price?"
          userUnderlying <- getLine
          putStrLn "Put or Call?"
          userPutCall <- getLine
          putStrLn "Volatility?"
          userVolatility <- getLine
          putStrLn "Expiry?"
          userExpiry <- getLine
          putStrLn "Interest Rate?"
          userInterestRate <- getLine
          putStrLn "Iterations?"
          userIterations <- getLine

          let userData = MonteCarloUserData { strike       = read(userStrike), 
                                              underlying   = read(userUnderlying), 
                                              putCall      = read(userPutCall),
                                              volatility   = read(userVolatility),  
                                              expiry       = read(userExpiry), 
                                              interestRate = read(userInterestRate), 
                                              iterations   = read(userIterations) }   

--- END OF COMMAND LINE INTERFACE


              -- Yuk, for QRNG we need to know our dimensionality
              -- before we start to simulate.

              -- THEROY 1 - +1 to any odd time steps
              -- This is rather nasty too, as Box Muller should
              -- only be used with EVEN time steps.  Consider
              -- if we have 3 time steps... on the final step
              -- we will generate TWO normals using bases 5,7.
              -- When we start our next simulation we will ask
              -- for a normal, and our Monad will inform us it
              -- has one saved.  But it generated with base 7,
              -- (our +1) not base 2.  So you get this sort of thing:
              -- 2 3 5 7 2 3 5 7 2 3 5 7 2 3 5 
              -- 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5
              --     ***   ***   ***   ***
              -- We are not using the same prime base for each
              -- timestep on each iteration. We cycle every 4
              -- simulations.  This is probably bad!
              -- Simple way to deal with is to destroy the final
              -- normal state after each single run... but this
              -- is NOT good for PRNG.
              -- As timeSteps goes large it becomes less of an 
              -- issue for PRNG, so we could ignore.
              
              -- For now it is up to the user to be sensible,
              -- the code below prevents the VERY disasterous
              -- scenario where we initalise Halton with say 
              -- 1 timestep and do not increment.  Thus we get
              -- the same base used for both rns for normal generation.
              -- This is because each timestep repeats the same prime
              -- sequence, which here will be 2,2,2,2,2....
              -- Thus adjacent sims are always used to generate each
              -- normal and we will use [2,2] as our base pair.  Not good!
              -- If we +1 we get points on adjacent sims being valued using
              -- a different base.  It will oscilate between 2 and 3.  But
              -- results show this is at least more acceptable.

              -- THEORY 2 - only +1 for disasterous single time step scenario
              -- For the case of >3 steps being used the situaion is less clear.
              -- Here there is cross-pollination for adjacent simulations but
              -- Only on the last (and first) time step.  So for 5 steps we get.
              -- 2 3 5 7 11 2 3 5 7 11 2 3 5 7 11
              -- 1 1 1 1 1  2 2 2 2 2  3 3 3 3 3
              --         ****       ****       *** and so on.
              -- On seconds thoughts as time steps go large this
              -- has to better than arbitarilly adding a base to odd realisations.  All
              -- but two of our timesteps are being valued using adjacent
              -- bases in the same simulation number.  So for now we will
              -- say if ts=1 then 2 otherwise ts.

              -- OK, don't ask me why but the odd+1 theory works much better in practice.
              -- It is also the method used in Brandimarte, 2006 (with no explanation).
              -- So let's stick with that!

-}