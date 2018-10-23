function dp = gossip2(t,p)
  dp = [0; 0; 0; 0];
  diffrate = 1.0;
  passrate = 0.05;
  prob = .25; 


dp(1) = -diffrate*min(1.0,5/max(1,p(1)+p(2)))*p(4)*p(1)+diffrate*min(1.0,5/max(1,p(1)+p(2)))*p(3)*p(2)-passrate*p(1)+diffrate*p(3);
dp(2) = -diffrate*min(1.0,5/max(1,p(1)+p(2)))*p(3)*p(2)+diffrate*min(1.0,5/max(1,p(1)+p(2)))*p(4)*p(1)-passrate*p(2)+diffrate*p(4);
dp(3) = +passrate*p(1)-diffrate*p(3);
dp(4) = +passrate*p(2)-diffrate*p(4);


