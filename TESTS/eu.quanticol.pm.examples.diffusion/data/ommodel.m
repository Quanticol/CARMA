function dp = ommodel(t,p)
  dp = [0; 0; 0; 0; 0; 0];
  lambda_sa = 0.1;
  lambda_ca = 0.1;
  lambda_sr = 1.0;
  p_sg = 0.75;
  p_b = 0.25;
  
  k = 10.0;

% CGP_INDEX = 0;
dp(1) = -lambda_sr*(1-p_sg)*p(1)... %CG+ - search_g! -> CB- 
        -lambda_sa*p(6)*k*p_b...*(p(1)/(p(1)+p(2)+p(3)+p(4))) %CG+ - bong*? -> CB-
        -lambda_ca*p(1)/(p(1)+p(2)+p(3)+p(4))*p(4)... %CG+ - shareG-? -> CB-
        -lambda_ca*p(1)/(p(1)+p(2)+p(3)+p(4))*p(3)... %CG+ - shareB+? -> CB+
        +lambda_ca*p(2)/(p(1)+p(2)+p(3)+p(4))*p(1)... %CG- shareG+? -> CG+
        +lambda_sr*p_sg*p(2)...%CG- - search_g! -> CG+ 
        +lambda_ca*p(3)/(p(1)+p(2)+p(3)+p(4))*p(1)... %CB+ - shareG+? -> CG+
        +lambda_ca*p(4)/(p(1)+p(2)+p(3)+p(4))*p(1)... %CB- shareG+? -> CG+
        ;

% CGM_INDEX = 1;
dp(2) = -lambda_sr*(1-p_sg)*p(2)... %CG- - search_g! -> CB- 
        -lambda_sr*p_sg*p(2)...%CG- - search_g! -> CG+ 
        -lambda_sa*p(6)*k*p_b...*(p(2)/(p(1)+p(2)+p(3)+p(4))) %CG- - bong*? -> CB-
        -lambda_ca*p(2)/(p(1)+p(2)+p(3)+p(4))*p(1)... %CG- shareG+? -> CG+
        -lambda_ca*p(2)/(p(1)+p(2)+p(3)+p(4))*p(4)... %CG- - shareG-? -> CB-
        -lambda_ca*p(2)/(p(1)+p(2)+p(3)+p(4))*p(3)... %CG- - shareB+? -> CB+
        +lambda_sr*(1-p_sg)*p(3)... %CB+ - search_b! -> CG-
        +lambda_sa*p(5)*k*p_b...*(p(3)/(p(1)+p(2)+p(3)+p(4))) %CB+ - giggle*? -> CG-
        +lambda_ca*p(3)/(p(1)+p(2)+p(3)+p(4))*p(2)... %CB+ - shareG-? -> CG-
        +lambda_sr*(1-p_sg)*p(4)... %CB- - search_g! -> CG- 
        +lambda_sa*p(5)*k*p_b...*(p(4)/(p(1)+p(2)+p(3)+p(4))) %CB- - giggle*? -> CG-
        +lambda_ca*p(4)/(p(1)+p(2)+p(3)+p(4))*p(2)... %CB- - shareB-? -> CG-
        ;
    
% CBP_INDEX = 2;
dp(3) = -lambda_sr*(1-p_sg)*p(3)... %CB+ - search_b! -> CG-
        -lambda_sa*p(5)*k*p_b...*(p(3)/(p(1)+p(2)+p(3)+p(4))) %CB+ - giggle*? -> CG-
        -lambda_ca*p(3)/(p(1)+p(2)+p(3)+p(4))*p(1)... %CB+ - shareG+? -> CG+
        -lambda_ca*p(3)/(p(1)+p(2)+p(3)+p(4))*p(2)... %CB+ - shareG-? -> CG-
        +lambda_ca*p(1)/(p(1)+p(2)+p(3)+p(4))*p(3)... %CG+ - shareB+? -> CB+
        +lambda_ca*p(2)/(p(1)+p(2)+p(3)+p(4))*p(3)... %CG- - shareB+? -> CB+
        +lambda_sr*p_sg*p(4)...%CB- - search_b! -> CB+ 
        +lambda_ca*p(4)/(p(1)+p(2)+p(3)+p(4))*p(3)... %CB- - shareB+? -> CB+
        ;

% CBM_INDEX = 3;
dp(4) = -lambda_sr*(1-p_sg)*p(4)... %CB- - search_b! -> CG- 
        -lambda_sr*p_sg*p(4)...%CB- - search_b! -> CB+ 
        -lambda_sa*p(5)*k*p_b...*(p(4)/(p(1)+p(2)+p(3)+p(4))) %CB- - giggle*? -> CG-
        -lambda_ca*p(4)/(p(1)+p(2)+p(3)+p(4))*p(1)... %CB- shareG+? -> CG+
        -lambda_ca*p(4)/(p(1)+p(2)+p(3)+p(4))*p(2)... %CB- - shareB-? -> CG-
        -lambda_ca*p(4)/(p(1)+p(2)+p(3)+p(4))*p(3)... %CB- - shareB+? -> CB+
        +lambda_sr*(1-p_sg)*p(1)... %CG+ - search_g! -> CB- 
        +lambda_sa*p_b*k*p(6)...*(p(1)/(p(1)+p(2)+p(3)+p(4))) %CG+ - bong*? -> CB-
        +lambda_ca*p(1)/(p(1)+p(2)+p(3)+p(4))*p(4)... %CG+ - shareG-? -> CB-
        +lambda_sr*(1-p_sg)*p(2)... %CG- - search_g! -> CB-         
        +lambda_sa*p(6)*k*p_b...*(p(2)/(p(1)+p(2)+p(3)+p(4))) %CG- - bong*? -> CB-
        +lambda_ca*p(2)/(p(1)+p(2)+p(3)+p(4))*p(4)... %CG- - shareG-? -> CB-
        ;

% G_INDEX = 4;
dp(5) = 0;

% B_INDEX = 5;  
dp(6) = 0;

  
