% Test our Haskell function in Octave
hs_init()
price_option(100,0.2,1,0.05,1,100000,'Halton','Box Muller','European')
hs_exit()
