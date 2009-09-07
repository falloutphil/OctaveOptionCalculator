% Test our Haskell function in Octave

hs_init()

T     = 1:-0.05:0;
S0    = 30:70;
K     = 50;
sigma = 0.4;
r     = 0.1;
[X,Y] = meshgrid(T,S0);
f = @(time,price) price_option(price,K,sigma,time,r,1,1000,'Halton','Box Muller','European');
surf(X,Y,f(X,Y))
% Need to pause whilst graph is shown or it will segv
pause()

hs_exit()
