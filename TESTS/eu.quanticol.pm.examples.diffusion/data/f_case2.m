function dp = f_case2(t,p)
  dp = [0; 0; 0];
  rate = .1; 
  k = 5.0;

dp(1) = -rate*min(1.0,(p(1)>0)*k/p(1))*p(2)*p(1);
dp(2) = rate*min(1.0,(p(1)>0)*k/p(1))*p(2)*p(1) -rate*p(2);
dp(3) = rate*p(2);


