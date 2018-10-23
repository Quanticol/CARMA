function dp = gossip1(t,p)
  dp = [0; 0; 0; 0];
  diffrate = 1.0;
  passrate = 0.05;
  prob = .25; 


dp(1) = -diffrate*prob*p(4)*p(1)+diffrate*prob*p(3)*p(2)-passrate*p(1)+diffrate*p(3);
dp(2) = -diffrate*prob*p(3)*p(2)+diffrate*prob*p(4)*p(2)-passrate*p(2)+diffrate*p(4);
dp(3) = +passrate*p(1)-diffrate*p(3);
dp(4) = +passrate*p(2)-diffrate*p(4);


