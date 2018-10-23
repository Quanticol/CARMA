function dp = u_gossip(t,p)
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
dp(3) = ... 
    - passrate*p(3) ... #beact
    + diffrate*(1-prob)*(1-prob)*p(1)*(p(3)/(p(3)+p(4))) ... #s_ii_00
    + diffrate*(1-prob)*(prob)*p(1)*(p(3)/(p(3)+p(4))) ... #s_ii_01
    - diffrate*(1-prob)*(prob)*p(1)*(p(3)/(p(3)+p(4))) ... #s_ii_01
    + diffrate*prob*prob*p(1)*(p(3)/(p(3)+p(4))) ... #s_ii_11
    + diffrate*(1-prob)*p(1)*(p(4)/(p(3)+p(4))) ... #s_iu_00
    + diffrate*prob*p(1)*p(4)/(p(3)+p(4)) ... #s_iu_10
    + diffrate*prob*p(2)*p(3)/(p(3)+p(4)) ... #s_ui_01
    - diffrate*prob*p(2)*p(3)/(p(3)+p(4)) ... #s_ui_01
    ; 

%Pu
dp(4) = ...
    - passrate*p(4) ...
    + diffrate*(1-prob)*(prob)*p(1)*(p(3)/(p(3)+p(4))) ... #s_ii_01
    + diffrate*prob*(1-prob)*p(1)*p(3)/(p(3)+p(4)) ... #s_ii_10
    + diffrate*prob*p(1)*p(4)/(p(3)+p(4)) ... #s_iu_10
    - diffrate*prob*p(1)*(p(4)/(p(3)+p(4))) ... #s_iu_10
    + diffrate*(1-prob)*p(2)*p(3)/(p(3)+p(4)) ... #s_ui_00
    + diffrate*prob*p(2)*p(3)/(p(3)+p(4)) ... #s_ui_01
    + diffrate*p(2)*p(4)/(p(3)+p(4)) ... #s_uu_00
    ; 






