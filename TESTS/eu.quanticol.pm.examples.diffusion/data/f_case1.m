function dp = f_case1(t,p)
  dp = [0; 0; 0];
  rate = .1; 
  prob = .2; 

dp(1) = -rate*prob*p(2)*p(1);
dp(2) = rate*prob*p(2)*p(1) -rate*p(2);
dp(3) = rate*p(2);


