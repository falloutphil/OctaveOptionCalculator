% Test our Haskell function in Octave

hs_init()

T     = 2:-0.05:0;
S0    = 20:80;
K     = 50;
sigma = 0.4;
r     = 0.1;
ts    = 1;
sims  = 1000;
[X,Y] = meshgrid(T,S0);
f = @(time,price) \
    price_option(price,K,sigma,time,r,ts,sims,'Halton','Box Muller','European');
surf(X,Y,f(X,Y))
title('Option Surface - Time to Expiry vs Initial Price vs Value')
xlabel('Time to Expiry')
ylabel('Initial Stock Price')
zlabel('Value')
% Need to pause whilst graph is shown or it will segv
pause()

hs_exit()
