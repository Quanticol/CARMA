function dp = acmodel(t,p)
  dp = [0; 0; 0; 0];
  lambda_a = 1.0;
  lambda_t = 1.0;
  p_t = 0.5;
  p_c = 0.5;
  
  k = 10.0;
  r=p(1);
  b=p(2);
  rt=p(3);
  bt=p(4);
  
  
% R_INDEX = 0;
dp(1) = -(r>0)*lambda_a*k*r/max(1.0,r+bt)*p_t*r...%A
        -(r>0)*lambda_a*k*r/max(1.0,r+bt)*p_t*rt...%B
        +(r+rt>0)*lambda_t*rt...%C
        -(r>0)*lambda_t*r/max(1.0,r+rt)*rt...%D
        +(b>0)*lambda_t*(b/max(1.0,b+bt))*bt...%E
        +(bt>0)*lambda_t*(bt/max(1.0,b+bt))*bt...%F
        +(rt>0)*lambda_a*p_c*k*rt/max(1.0,rt+b)*(b+bt)...%G
    ;


% B_INDEX = 1;
dp(2) = -(b>0)*lambda_a*k*b/max(1.0,b+rt)*p_t*b...%H
        -(b>0)*lambda_a*k*b/max(1.0,(b+rt))*p_t*bt...%I
        +(b+bt>0)*lambda_t*bt...
        +(r>0)*lambda_t*r/max(1.0,r+rt)*rt...%D
        -(b>0)*lambda_t*b/max(1.0,b+bt)*bt...%E
        +(rt>0)*lambda_t*(rt/max(1.0,r+rt))*rt...%L   
        +(bt>0)*lambda_a*p_c*k*(bt/max(1.0,r+bt))*(r+rt)...%M
        ;
    
% RT_INEDX = 2;
dp(3) = -(rt>0)*lambda_a*p_c*k*rt/max(1.0,rt+b)*(b+bt)...%G    
        -(r+rt>0)*lambda_t*rt...%C
        -(rt>0)*lambda_t*(rt/max(1.0,r+rt))*rt...%L   
        +(r>0)*lambda_a*k*(r/max(1.0,r+bt))*p_t*r...%A
        +(r>0)*lambda_a*k*(r/max(1.0,r+bt))*p_t*rt...%B
        ;
    

% BT_INDEX = 3;
dp(4) = -(bt>0)*lambda_a*k*(bt/max(1.0,r+bt))*p_c*(r+rt)...%M
        -(b+bt>0)*lambda_t*bt...
        -(bt>0)*lambda_t*(bt/max(1.0,b+bt))*bt...%F
        +(b>0)*lambda_a*k*b/max(1.0,b+rt)*p_t*b...%H
        +(b>0)*lambda_a*k*b/max(1.0,b+rt)*p_t*bt...%I 
      ;

  