function dp = b_gossip(t,p)
  dp = [0; 0; 0; 0];
  diffrate = 1.0;
  passrate = 0.05;
  prob = 0.01; 
  k = 10;

%Ai
dp(1) = -diffrate*p(1)+passrate*p(3);

%Au
dp(2) = -diffrate*p(2)+passrate*p(4);

%Pi
dp(3) = +diffrate*p(1)-passrate*p(3)+diffrate*prob*p(1)*k*p(4)/(p(3)+p(4))-diffrate*prob*p(2)*k*p(3)/(p(3)+p(4))-diffrate*(1-prob)*prob*p(1)*k*p(3)/(p(3)+p(4))

%Pu
dp(4) =  +diffrate*p(2)-passrate*p(4)-diffrate*prob*p(1)*k*p(4)/(p(3)+p(4))+diffrate*prob*p(2)*k*p(3)/(p(3)+p(4))+diffrate*(1-prob)*prob*p(1)*k*p(3)/(p(3)+p(4))




