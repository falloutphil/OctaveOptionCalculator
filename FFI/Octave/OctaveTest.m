% Test our Haskell functions in Octave

% Init Haskel runtime
% If you are running this manually
% inside octave then comment out
% init and exit functions and run
% them manually once per session.
% http://hackage.haskell.org/trac/ghc/ticket/2863
hs_init()

% Option Parameters
n = 50;
expiries     = linspace(0,2,n);
underlying   = linspace(20,80,n); 
strike       = 50;
vol          = 0.4;
ir           = 0.1;
ts_euro      = 1;
ts_lookback  = 24;
sims         = 1000;

% Create grid of 'time to expiry' vs 'initial underlying'
[X,Y] = meshgrid(expiries,underlying);

% Create option functions with 2 variables for plotting
european = @(expiry,underl) \
           price_option(underl,strike,vol,expiry,ir,ts_euro,sims,'Call','Halton','Box Muller','European');

lookback = @(expiry,underl) \
           price_option(underl,strike,vol,expiry,ir,ts_lookback,sims,'Call','Ranq1','Acklam','Lookback');

% European calculation and plot
subplot(2,1,1);
Z1 = european(X,Y);
% Must surf BEFORE we add labels!
surf(X,Y,Z1,Z1)
title('European Call Option Surface - Struck at 50')
xlabel('Time to Expiry')
ylabel('Initial Stock Price')
zlabel('Value')
shading faceted
colorbar

% Lookback calculation and plot
subplot(2,1,2);
Z2 = lookback(X,Y);
surf(X,Y,Z2,Z2)
title('Lookback Call Option Surface - Struck at 50')
xlabel('Time to Expiry')
ylabel('Initial Stock Price')
zlabel('Value')
shading faceted
colorbar

% Write result to file
print('OctaveTest.png', '-S1280,960')

% Exit Haskell runtime
hs_exit()
