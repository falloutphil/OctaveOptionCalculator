% Test our Haskell function in Octave
hs_init()
% Strike=100, Vol=20%, T=1, IR=5%, Time Steps=1, Sims=100000
price_option(100,0.2,1,0.05,1,100000,'Halton','Box Muller','European')
hs_exit()
