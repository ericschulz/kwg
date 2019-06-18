m=csvread('/home/hanshalbe/diffevidence.csv');
[alpha,exp_r,xp,pxp,bor] = bms(-m);
probofexp=pxp;
bayesianomni=bor;