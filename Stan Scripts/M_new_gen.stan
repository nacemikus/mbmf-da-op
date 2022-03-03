data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  int<lower=1, upper=4> level1_choice[N,T];  // 1: 1 - 4
  int<lower=1, upper=2> state1[N,T];  // 1,2 
  int<lower=1, upper=2> state2[N,T];  // 1,2 
  int<lower=1, upper=4> stim_left[N,T];  // 1-4
  int<lower=-4, upper=5> reward[N,T];
  
  int<lower=1, upper=T> Tsubj_t0[N];
  int<lower=-1, upper=4> level1_choice_t0[N,T];  // 1: 1 - 4
  int<lower=-1, upper=2> state1_t0[N,T];  // 1,2 
  int<lower=-1, upper=2> state2_t0[N,T];  // 1,2 
  int<lower=1, upper=4> stim_left_t0[N,T];  // 1-4
  
  int<lower=-4, upper=5> reward_t0[N,T];
  
  int<lower=0, upper=1> naltrexone[N];
  int<lower=0, upper=1> amisulpride[N];
  real dat[N];
  real ankk[N];
  real darpp[N];
  real comt[N];
}
transformed data {
}
parameters {
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[8] mu_p;
  vector<lower=0>[8] sigma;
  matrix[8, N] z;
  cholesky_factor_corr[8] L_Omega;
  // fixed parameters
  // real beta_nal;
  // real beta_ami;
  // 
  // real beta_nal_ankk;
  // real beta_ami_ankk;
  // real beta_nal_dat;
  // real beta_ami_dat;
  // real beta_nal_comt;
  // real beta_ami_comt;
  // real beta_nal_darpp;
  // real beta_ami_darpp;
  // 
  // real beta_ankk;
  // 
  // real beta_dat;
  // 
  // real beta_comt;
  // 
  // real beta_darpp;  
  
  real beta_nal_mb;
  real beta_ami_mb;
   real beta_nal_mf;
  real beta_ami_mf;
  real beta_nal_noise;
  real beta_ami_noise;
  real beta_nal_g;
  real beta_ami_g;
  
  real beta_mix1_nal_ankk_mb;
  real beta_mix1_ami_ankk_mb;
  real beta_mix1_nal_dat_mb;
  real beta_mix1_ami_dat_mb;
  real beta_mix1_nal_comt_mb;
  real beta_mix1_ami_comt_mb;
  real beta_mix1_nal_darpp_mb;
  real beta_mix1_ami_darpp_mb;
 
  real beta_mix1_ankk_mb;
  
  real beta_mix1_dat_mb;
  
  real beta_mix1_comt_mb;
  
  real beta_mix1_darpp_mb;  
  
  real beta_mix1_nal_ankk_mf;
  real beta_mix1_ami_ankk_mf;
  real beta_mix1_nal_dat_mf;
  real beta_mix1_ami_dat_mf;
  real beta_mix1_nal_comt_mf;
  real beta_mix1_ami_comt_mf;
  real beta_mix1_nal_darpp_mf;
  real beta_mix1_ami_darpp_mf;
 
  real beta_mix1_ankk_mf;
  
  real beta_mix1_dat_mf;
  
  real beta_mix1_comt_mf;
  
  real beta_mix1_darpp_mf;  
  //  real beta_mix2_nal;
  // real beta_mix2_ami;
  
  real beta_mix2_nal_ankk_mb;
  real beta_mix2_ami_ankk_mb;
  real beta_mix2_nal_dat_mb;
  real beta_mix2_ami_dat_mb;
  real beta_mix2_nal_comt_mb;
  real beta_mix2_ami_comt_mb;
  real beta_mix2_nal_darpp_mb;
  real beta_mix2_ami_darpp_mb;
  
  real beta_mix2_ankk_mb;
  
  real beta_mix2_dat_mb;
  
  real beta_mix2_comt_mb;
  
  real beta_mix2_darpp_mb;  
  
  real beta_mix2_nal_ankk_mf;
  real beta_mix2_ami_ankk_mf;
  real beta_mix2_nal_dat_mf;
  real beta_mix2_ami_dat_mf;
  real beta_mix2_nal_comt_mf;
  real beta_mix2_ami_comt_mf;
  real beta_mix2_nal_darpp_mf;
  real beta_mix2_ami_darpp_mf;
  
  real beta_mix2_ankk_mf;
  
  real beta_mix2_dat_mf;
  
  real beta_mix2_comt_mf;
  
  real beta_mix2_darpp_mf;  
  // 
  // real tau_sq_nal;
  // real tau_sq_ami;

  real<lower=0> tau_sq_nal_ankk_mb;
  real<lower=0> tau_sq_ami_ankk_mb;
  real<lower=0> tau_sq_nal_dat_mb;
  real<lower=0> tau_sq_ami_dat_mb;
  real<lower=0> tau_sq_nal_comt_mb;
  real<lower=0> tau_sq_ami_comt_mb;
  real<lower=0> tau_sq_nal_darpp_mb;
  real<lower=0> tau_sq_ami_darpp_mb;
  //
  real<lower=0> tau_sq_ankk_mb;

  real<lower=0> tau_sq_dat_mb;

  real<lower=0> tau_sq_comt_mb;

  real<lower=0> tau_sq_darpp_mb;
  
   real<lower=0> tau_sq_nal_ankk_mf;
  real<lower=0> tau_sq_ami_ankk_mf;
  real<lower=0> tau_sq_nal_dat_mf;
  real<lower=0> tau_sq_ami_dat_mf;
  real<lower=0> tau_sq_nal_comt_mf;
  real<lower=0> tau_sq_ami_comt_mf;
  real<lower=0> tau_sq_nal_darpp_mf;
  real<lower=0> tau_sq_ami_darpp_mf;
  //
  real<lower=0> tau_sq_ankk_mf;

  real<lower=0> tau_sq_dat_mf;

  real<lower=0> tau_sq_comt_mf;

  real<lower=0> tau_sq_darpp_mf;
  // 
  // real gamma_nal;
  // real  gamma_ami;
  
  real <lower=0, upper =1>  gamma_nal_ankk_mb;
  real <lower=0, upper =1>  gamma_ami_ankk_mb;
  real <lower=0, upper =1>  gamma_nal_dat_mb;
  real <lower=0, upper =1>  gamma_ami_dat_mb;
  real <lower=0, upper =1>  gamma_nal_comt_mb;
  real <lower=0, upper =1>  gamma_ami_comt_mb;
  real <lower=0, upper =1>  gamma_nal_darpp_mb;
  real <lower=0, upper =1>  gamma_ami_darpp_mb;
  
  real <lower=0, upper =1>  gamma_ankk_mb;
  
  real <lower=0, upper =1>  gamma_dat_mb;
  
  real <lower=0, upper =1>  gamma_comt_mb;
  
  real <lower=0, upper =1>  gamma_darpp_mb;  
  
  real <lower=0, upper =1>  gamma_nal_ankk_mf;
  real <lower=0, upper =1>  gamma_ami_ankk_mf;
  real <lower=0, upper =1>  gamma_nal_dat_mf;
  real <lower=0, upper =1>  gamma_ami_dat_mf;
  real <lower=0, upper =1>  gamma_nal_comt_mf;
  real <lower=0, upper =1>  gamma_ami_comt_mf;
  real <lower=0, upper =1>  gamma_nal_darpp_mf;
  real <lower=0, upper =1>  gamma_ami_darpp_mf;
  
  real <lower=0, upper =1>  gamma_ankk_mf;
  
  real <lower=0, upper =1>  gamma_dat_mf;
  
  real <lower=0, upper =1>  gamma_comt_mf;
  
  real <lower=0, upper =1>  gamma_darpp_mf;  
  
}
transformed parameters {
  // Transform subject-level raw parameters
  // real beta_nal;
  // real beta_ami;
  
  real beta_nal_ankk_mb;
  real beta_ami_ankk_mb;
  real beta_nal_dat_mb;
  real beta_ami_dat_mb;
  real beta_nal_comt_mb;
  real beta_ami_comt_mb;
  real beta_nal_darpp_mb;
  real beta_ami_darpp_mb;
  
  real beta_ankk_mb;
  
  real beta_dat_mb;
  
  real beta_comt_mb;
  
  real beta_darpp_mb;
  
  real beta_nal_ankk_mf;
  real beta_ami_ankk_mf;
  real beta_nal_dat_mf;
  real beta_ami_dat_mf;
  real beta_nal_comt_mf;
  real beta_ami_comt_mf;
  real beta_nal_darpp_mf;
  real beta_ami_darpp_mf;
  
  real beta_ankk_mf;
  
  real beta_dat_mf;
  
  real beta_comt_mf;
  
  real beta_darpp_mf;
  
  real phi_scale;
  
  vector[N] g_mean;
  vector[N]         beta1_mean;
  
  vector[N] w_mb;
  vector[N] w_mf;
  
  vector<lower=0,upper=1>[N] g;
  vector<lower=0>[N]         beta1;
  
  vector<lower=0,upper=1>[N] w;
  
  
  vector<lower=0,upper=1>[N] g_t0;
  vector<lower=0>[N]         beta1_t0;
  
  // vector<lower=0,upper=5>[N] pi;
  vector<lower=0,upper=1>[N] w_t0;
  
  
  vector[N] sess_g;
  vector[N] sess_beta1;
  
  vector[N] sess_w;
  matrix[6, N] r1;
  
  r1 = (diag_pre_multiply(sigma,L_Omega) * z);
  
  phi_scale = 0.001;
  
  // beta_nal = gamma_nal*beta_mix1_nal + (1-gamma_nal)*beta_mix2_nal;
  // beta_ami = gamma_ami*beta_mix1_ami + (1-gamma_ami)*beta_mix2_ami;
  
  
  beta_nal_ankk = gamma_nal_ankk*beta_mix1_nal_ankk + (1-gamma_nal_ankk)*beta_mix2_nal_ankk;
  beta_nal_comt = gamma_nal_comt*beta_mix1_nal_comt + (1-gamma_nal_comt)*beta_mix2_nal_comt;
  beta_nal_darpp = gamma_nal_darpp*beta_mix1_nal_darpp + (1-gamma_nal_darpp)*beta_mix2_nal_darpp;
  beta_nal_dat = gamma_nal_dat*beta_mix1_nal_dat + (1-gamma_nal_dat)*beta_mix2_nal_dat;
  
  beta_ami_dat = gamma_ami_dat*beta_mix1_ami_dat + (1-gamma_ami_dat)*beta_mix2_ami_dat;
  beta_ami_comt = gamma_ami_comt*beta_mix1_ami_comt + (1-gamma_ami_comt)*beta_mix2_ami_comt;
  beta_ami_darpp = gamma_ami_darpp*beta_mix1_ami_darpp + (1-gamma_ami_darpp)*beta_mix2_ami_darpp;
  beta_ami_ankk = gamma_ami_ankk*beta_mix1_ami_ankk + (1-gamma_ami_ankk)*beta_mix2_ami_ankk;
  
  beta_ankk = gamma_ankk*beta_mix1_ankk + (1-gamma_ankk)*beta_mix2_ankk;
  beta_dat = gamma_dat*beta_mix1_dat + (1-gamma_dat)*beta_mix2_dat;
  beta_comt = gamma_comt*beta_mix1_comt + (1-gamma_comt)*beta_mix2_comt;
  beta_darpp = gamma_darpp*beta_mix1_darpp + (1-gamma_darpp)*beta_mix2_darpp;
  
  for (i in 1:N) {
    
    
    
    beta1_mean[i]  =  mu_p[1] + r1[1,i];
    w_mean[i]      =  mu_p[2] + r1[2,i] +
    beta_ankk*ankk[i] + beta_dat*dat[i] +
    beta_comt*comt[i] + beta_darpp*darpp[i];
    
    g_mean[i] =    mu_p[3] + r1[3,i];
    
    
    sess_beta1[i] = mu_p[4] + r1[4,i];
    sess_w[i] = mu_p[5] + r1[5,i] + 
    (beta_nal_ankk*ankk[i] + beta_nal_comt*comt[i] + 
    beta_nal_darpp*darpp[i] + beta_nal_dat*dat[i] +beta_nal)*naltrexone[i] + 
    (beta_ami_ankk*ankk[i] + beta_ami_comt*comt[i] + 
    beta_ami_darpp*darpp[i] + beta_ami_dat*dat[i] +beta_ami)*amisulpride[i];
    
    
    sess_g[i] = mu_p[6] + r1[6,i];
    
    
    beta1_t0[i]  = exp( beta1_mean[i]   );
    w_t0[i]      = Phi_approx( w_mean[i] );
    
    g_t0[i] =   Phi_approx(  g_mean[i]  );
    
    
    
    beta1[i]  = exp( beta1_mean[i]   + sess_beta1[i] );
    w[i]     =  Phi_approx( w_mean[i] + sess_w[i] );
    
    g[i] =Phi_approx(  g_mean[i]  + sess_g[i]  );
    
    
  }
}
model {
  // Hyperparameters
  mu_p  ~ normal(0, 1.5);
  // mu_p[7]  ~ normal(0, 1);
  // mu_p[8]  ~ normal(0, 10);
  sigma ~ normal(0,1);
  // sigma[1] ~ normal(0,0.2); // beta
  // sigma[2] ~ normal(0,0.2);
  // sigma[3] ~ cauchy(0,2);
  // sigma[4] ~ cauchy(0,2);
  // sigma[5] ~ cauchy(0,2);
  // sigma[6] ~ cauchy(0,2);
  // 
  // sigma[7] ~ normal(0,0.2); // beta session
  // sigma[8] ~ cauchy(0,2);
  // sigma[9] ~ cauchy(0,2);
  // sigma[10] ~ cauchy(0,2);
  // sigma[11] ~ cauchy(0,2);
  // sigma[12] ~ cauchy(0,2);
  // fixed parameters
  
  beta_nal ~ normal(0,1.5);
  beta_ami ~ normal(0,1.5);
  
  gamma_nal_ankk ~ uniform(0,1.0);
  tau_sq_nal_ankk ~  inv_gamma(0.5,0.5);
  beta_mix1_nal_ankk ~   normal(0,tau_sq_nal_ankk);
  beta_mix2_nal_ankk ~ normal(0,phi_scale);
  
  gamma_nal_dat ~ uniform(0,1.0);
  tau_sq_nal_dat ~  inv_gamma(0.5,0.5);
  beta_mix1_nal_dat ~  normal(0,tau_sq_nal_dat);
  beta_mix2_nal_dat ~ normal(0,phi_scale);
  
  gamma_nal_darpp ~ uniform(0,1.0);
  tau_sq_nal_darpp ~  inv_gamma(0.5,0.5);
  beta_mix1_nal_darpp ~  normal(0,tau_sq_nal_darpp);
  beta_mix2_nal_darpp ~ normal(0,phi_scale);
  
  gamma_nal_comt ~ uniform(0,1.0);
  tau_sq_nal_comt ~  inv_gamma(0.5,0.5);
  beta_mix1_nal_comt ~  normal(0,tau_sq_nal_comt);
  beta_mix2_nal_comt ~ normal(0,phi_scale);
  
  gamma_ami_ankk ~ uniform(0,1.0);
  tau_sq_ami_ankk ~  inv_gamma(0.5,0.5);
  beta_mix1_ami_ankk ~   normal(0,tau_sq_ami_ankk);
  beta_mix2_ami_ankk ~ normal(0,phi_scale);
  
  gamma_ami_dat ~ uniform(0,1.0);
  tau_sq_ami_dat ~  inv_gamma(0.5,0.5);
  beta_mix1_ami_dat ~  normal(0,tau_sq_ami_dat);
  beta_mix2_ami_dat ~ normal(0,phi_scale);
  
  gamma_ami_comt ~ uniform(0,1.0);
  tau_sq_ami_comt ~  inv_gamma(0.5,0.5);
  beta_mix1_ami_comt ~  normal(0,tau_sq_ami_comt);
  beta_mix2_ami_comt ~ normal(0,phi_scale);
  
  gamma_ami_darpp ~ uniform(0,1.0);
  tau_sq_ami_darpp ~  inv_gamma(0.5,0.5);
  beta_mix1_ami_darpp ~   normal(0,tau_sq_ami_darpp);
  beta_mix2_ami_darpp ~ normal(0,phi_scale);
  
  gamma_darpp ~ uniform(0,1.0);
  tau_sq_darpp ~  inv_gamma(0.5,0.5);
  beta_mix1_darpp ~   normal(0,tau_sq_darpp);
  beta_mix2_darpp ~ normal(0,phi_scale);
  
  gamma_comt ~ uniform(0,1.0);
  tau_sq_comt ~  inv_gamma(0.5,0.5);
  beta_mix1_comt ~  normal(0,tau_sq_comt);
  beta_mix2_comt ~ normal(0,phi_scale);
  
  gamma_dat ~ uniform(0,1.0);
  tau_sq_dat ~  inv_gamma(0.5,0.5);
  beta_mix1_dat ~  normal(0,tau_sq_dat);
  beta_mix2_dat ~ normal(0,phi_scale);
  
  gamma_ankk ~ uniform(0,1.0);
  tau_sq_ankk ~  inv_gamma(0.5,0.5);
  beta_mix1_ankk ~  normal(0,tau_sq_ankk);
  beta_mix2_ankk ~ normal(0,phi_scale);
  
  
  // individual parameters
  
  to_vector(z) ~  normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  
  
  for (i in 1:N) {
    // Define values
    vector[2] v_mb;    // model-based stimulus values for level 1 (2 stimuli)
    matrix[2,2] v_mf;    // model-free stimulus values for level 1&2 (1,2--> level 1, 3-6--> level 2)
    vector[2] v_2;
    vector[2] v_hybrid;  // hybrid stimulus values for level 1 (2 stimuli)
    real level1_prob_choice2; // Initialize prob. of choosing stim 2 (0 or 1) in level 1
    matrix[2, 2] trans_prob_state1_1;
    matrix[2, 2] trans_prob_state1_2;
    int trans_prob_state1_1_changed;
    int trans_prob_state1_2_changed;
    int choice;
    int choice01;
    int action_right;
    int action_left;
    int pressed_left_prev;
    
    real a1_par;
    real a1_g_par;
    real a1_l_par;
    real a2_par;
    real g_par;
    real st_par;
    real w_par;
    real delta;
    
    w_par = w[i];
    a1_g_par = 1;
    a1_l_par = 1;
    a2_par =1;
    g_par = g[i];
    st_par = 0;
    
    
    // Initialize values
    trans_prob_state1_1 = rep_matrix(0.5, 2,2);
    trans_prob_state1_2 = rep_matrix(0.5, 2,2);
    v_mb  = rep_vector(0.0, 2);
    v_mf  = rep_matrix(0.0, 2,2);
    v_2 = rep_vector(0.0,2);
    v_hybrid = rep_vector(0.0, 2);
    trans_prob_state1_1_changed = 0;
    trans_prob_state1_2_changed = 0;
    
    for (t in 1:Tsubj[i])  {
      
      // mark that the agent has learned the state transfer
      if (state1[i,t] == 1 && trans_prob_state1_1_changed== 0) {
        trans_prob_state1_1_changed = 1;
      } else if (state1[i,t] == 2 && trans_prob_state1_2_changed== 0) {
        trans_prob_state1_2_changed = 1;
      }
      
      // compute v_mb 
      //    Qmb = Tm{s1}'*Q2;     
      if (state1[i,t]==1) {
        v_mb = trans_prob_state1_1 * v_2;
      } else if (state1[i,t]==2) {
        v_mb = trans_prob_state1_2 * v_2;
      }
      
      
      
      // compute v_hybrid
      //    Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
      v_hybrid[1] = w_par * v_mb[1] + (1-w_par) * v_mf[state1[i,t],1];   // Q of choosing 1,3 (state 2 = 1) 
      v_hybrid[2] = w_par * v_mb[2] + (1-w_par) * v_mf[state1[i,t],2];   // Q of choosing 2,4 (state 2 = 2)
      
      // set the choice from 1-4 to 1-2
      choice = level1_choice[i,t];
      if(choice > 2){
        choice = choice - 2;
      }
      
      // def choice for bernoulli
      choice01 = choice -1;  
      
      //    
      
      
      
      //  agent realizes transition structure:
      if (trans_prob_state1_1_changed == 1 && state1[i,t] == 1) {
        trans_prob_state1_1 = [[1, 0],[0, 1]];
      }         //  agent realizes transition structure
      
      if (trans_prob_state1_2_changed == 1 && state1[i,t] == 2) {
        trans_prob_state1_2 = [[1, 0],[0, 1]];
      } 
      
      // make the last response sticky
      
      if(!(t == 1)) {
        pressed_left_prev = stim_left[i,t-1] == level1_choice[i,t-1];
        action_left = stim_left[i,t];
        if (action_left > 2) action_left = action_left- 2;
        
        action_right = 3 - action_left; 
        
        if(pressed_left_prev == 1) {
          v_hybrid[action_left] =  v_hybrid[action_left] + st_par;
        } else {
          v_hybrid[action_right] =  v_hybrid[action_right] + st_par;
        }
        
      }
      
      
      level1_prob_choice2 = inv_logit( beta1[i]*(v_hybrid[2]-v_hybrid[1]));
      // level1_prob_choice2 = beta1[i]*(v_hybrid[2]-v_hybrid[1]);
      
      choice01 ~ bernoulli(level1_prob_choice2 );  // level 1, prob. of choosing 2 in level 1
      
      // alternative model formulation
      if (is_nan(sum(beta1[i]*v_hybrid))) {
        print("beta is ", beta1[i], ", vh[1] is ", v_hybrid[1], ", vh[2] is ", v_hybrid[1] );
      }
      // choice ~ categorical_logit(beta1[i]*v_hybrid);
      
      
      // Observe Level2 and update Level1 of the chosen option
      //    dtQ(2) = subdata.points(t) - Q2(s2);                            % prediction error (2nd choice)
      delta = reward[i,t] - v_mf[state1[i,t], choice];
      if (delta >0) {
        a1_par = a1_g_par;
      } else {
        a1_par = a1_l_par;
      }
      v_mf[state1[i,t], choice] = v_mf[state1[i,t], choice] + a1_par*delta;
      
      // After observing the reward at Level 2...
      // Update Level 2 v_mf of the chosen option. Level 2--> choose one of level 2 options and observe reward
      //    Q2(s2) = Q2(s2) + lr*dtQ(2);            
      v_2[state2[i,t]] =v_2[state2[i,t]] + a2_par*(reward[i,t] - v_2[state2[i,t]] );
      
      // Update Level 1 v_mf with eligibility trace
      //    Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);   
      // v_mf[state1[i,t], choice] = v_mf[state1[i,t], choice] + lambda[i] * a1[i] * (reward[i,t] - v_2[state2[i,t]] );
      // forget others
      
      v_mf[state1[i,t], 3-choice] = (1-g_par)*v_mf[state1[i,t], 3-choice];
      v_mf[3-state1[i,t], choice] = (1-g_par)*v_mf[3-state1[i,t], choice];
      v_mf[3-state1[i,t], 3-choice] = (1-g_par )*v_mf[3-state1[i,t], 3-choice];
      
      
    } // end of t loop
    
    if (Tsubj_t0[i]!= 1) {
      
      ////////////////////////////////////// start with t0
      vector[2] v_mb_t0;    // model-based stimulus values for level 1 (2 stimuli)
      matrix[2,2] v_mf_t0;    // model-free stimulus values for level 1&2 (1,2--> level 1, 3-6--> level 2)
      vector[2] v_2_t0;
      vector[2] v_hybrid_t0;  // hybrid stimulus values for level 1 (2 stimuli)
      real level1_prob_choice2_t0; // Initialize prob. of choosing stim 2 (0 or 1) in level 1
      matrix[2, 2] trans_prob_state1_1_t0;
      matrix[2, 2] trans_prob_state1_2_t0;
      int trans_prob_state1_1_changed_t0;
      int trans_prob_state1_2_changed_t0;
      int choice_t0;
      int choice01_t0;
      int action_right_t0;
      int action_left_t0;
      int pressed_left_prev_t0;
      
      
      real a1_t0_par;
      real a1_g_t0_par;
      real a1_l_t0_par;
      real a2_t0_par;
      real g_t0_par;
      real st_t0_par;
      real w_t0_par;
      
      w_t0_par=w_t0[i];
      a1_g_t0_par = 1;
      a1_l_t0_par = 1;
      a2_t0_par =1;
      g_t0_par = g_t0[i];
      st_t0_par =0;
      // Initialize values
      trans_prob_state1_1_t0 = rep_matrix(0.5, 2,2);
      trans_prob_state1_2_t0 = rep_matrix(0.5, 2,2);
      v_mb_t0  = rep_vector(0.0, 2);
      v_mf_t0  = rep_matrix(0.0, 2,2);
      v_2_t0 = rep_vector(0.0,2);
      v_hybrid_t0 = rep_vector(0.0, 2);
      trans_prob_state1_1_changed_t0 = 0;
      trans_prob_state1_2_changed_t0 = 0;
      
      for (t in 1:Tsubj_t0[i])  {
        
        // mark that the agent has learned the state transfer
        if (state1_t0[i,t] == 1 && trans_prob_state1_1_changed_t0== 0) {
          trans_prob_state1_1_changed_t0 = 1;
        } else if (state1_t0[i,t] == 2 && trans_prob_state1_2_changed_t0== 0) {
          trans_prob_state1_2_changed_t0 = 1;
        }
        
        // compute v_mb 
        //    Qmb = Tm{s1}'*Q2;     
        if (state1_t0[i,t]==1) {
          v_mb_t0 = trans_prob_state1_1_t0 * v_2_t0;
        } else if (state1_t0[i,t]==2) {
          v_mb_t0 = trans_prob_state1_2_t0 * v_2_t0;
        }
        
        
        
        // compute v_hybrid
        //    Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
        v_hybrid_t0[1] = w_t0_par * v_mb_t0[1] + (1-w_t0_par) * v_mf_t0[state1_t0[i,t],1];   // Q of choosing 1,3 (state 2 = 1) 
        v_hybrid_t0[2] = w_t0_par * v_mb_t0[2] + (1-w_t0_par) * v_mf_t0[state1_t0[i,t],2];   // Q of choosing 2,4 (state 2 = 2)
        
        // set the choice from 1-4 to 1-2
        choice_t0 = level1_choice_t0[i,t];
        if(choice_t0 > 2){
          choice_t0 = choice_t0 - 2;
        }
        
        // def choice for bernoulli
        choice01_t0 = choice_t0 -1;  
        
        //    
        
        
        
        //  agent realizes transition structure:
        if (trans_prob_state1_1_changed_t0 == 1 && state1_t0[i,t] == 1) {
          trans_prob_state1_1_t0 = [[1, 0],[0, 1]];
        }         //  agent realizes transition structure
        
        if (trans_prob_state1_2_changed_t0 == 1 && state1_t0[i,t] == 2) {
          trans_prob_state1_2_t0 = [[1, 0],[0, 1]];
        } 
        
        // make the last choice sticky
        if(!(t == 1)) {
          pressed_left_prev_t0 = stim_left_t0[i,t-1] == level1_choice_t0[i,t-1];
          action_left_t0 = stim_left_t0[i,t];
          if (action_left_t0 > 2) action_left_t0 = action_left_t0- 2;
          action_right_t0 = 3 - action_left_t0; 
          
          if(pressed_left_prev_t0 == 1) {
            v_hybrid_t0[action_left_t0] =  v_hybrid_t0[action_left_t0] + st_t0_par;
          } else {
            v_hybrid_t0[action_right_t0] =  v_hybrid_t0[action_right_t0] + st_t0_par;
          }
          
        }
        
        
        level1_prob_choice2_t0 = inv_logit( beta1_t0[i]*(v_hybrid_t0[2]-v_hybrid_t0[1]));
        
        choice01_t0 ~ bernoulli(level1_prob_choice2_t0);  // level 1, prob. of choosing 2 in level 1
        
        // alternative model formulation
        // choice_t0 ~ categorical_logit( beta1_t0[i]*v_hybrid_t0);
        
        // Observe Level2 and update Level1 of the chosen option
        //    dtQ(2) = subdata.points(t) - Q2(s2);                            % prediction error (2nd choice)
        delta = reward_t0[i,t] - v_mf_t0[state1_t0[i,t], choice_t0];
        if (delta >0) {
          a1_t0_par = a1_g_t0_par;
        } else {
          a1_t0_par = a1_l_t0_par;
        }
        v_mf_t0[state1_t0[i,t], choice_t0] = v_mf_t0[state1_t0[i,t], choice_t0] + a1_t0_par*delta;
        
        // After observing the reward at Level 2...
        // Update Level 2 v_mf of the chosen option. Level 2--> choose one of level 2 options and observe reward
        //    Q2(s2) = Q2(s2) + lr*dtQ(2);            
        v_2_t0[state2_t0[i,t]] =v_2_t0[state2_t0[i,t]] + a2_t0_par*(reward_t0[i,t] - v_2_t0[state2_t0[i,t]] );
        
        // forget others
        
        v_mf_t0[state1_t0[i,t], 3-choice_t0] = (1-g_t0_par)*v_mf_t0[state1_t0[i,t], 3-choice_t0];
        v_mf_t0[3-state1_t0[i,t], choice_t0] = (1-g_t0_par)*v_mf_t0[3-state1_t0[i,t], choice_t0];
        v_mf_t0[3-state1_t0[i,t], 3-choice_t0] = (1-g_t0_par)*v_mf_t0[3-state1_t0[i,t], 3-choice_t0];
        
        
        
        // Update Level 1 v_mf with eligibility trace
        //    Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);   
        // v_mf_t0[state1_t0[i,t], choice_t0] = v_mf_t0[state1_t0[i,t], choice_t0] + lambda_t0[i] * a1_t0[i] * (reward_t0[i,t] - v_2_t0[state2_t0[i,t]] );
        
        
      } // end of t loop
      ////////////////////////////////////////// end of t0
      
      
      
      
    } // end of if Tsubj_t0[i] != 1;
    
  } // end of i loop
}

generated quantities {
  // For group level parameters
  
  // real<lower=0>         mu_beta1;
  // // real<lower=0,upper=1> mu_a2;
  // // real<lower=0>         mu_beta2;
  // // real<lower=0,upper=5> mu_pi;
  // real<lower=0,upper=1> mu_w;
  // // real<lower=0,upper=1> mu_lambda;
  // real<lower=0,upper=1> mu_a1_t0;
  // real<lower=0>         mu_beta1_t0;
  // // real<lower=0,upper=1> mu_a2_t0;
  // // real<lower=0>         mu_beta2;
  // // real<lower=0,upper=5> mu_pi;
  // real<lower=0,upper=1> mu_w_t0;
  //  real mu_st_t0;
  //  real mu_st;
  // real<lower=0,upper=1> mu_lambda_t0;
  
  // real mu_sess_a1;
  // real mu_sess_beta1;
  // real mu_sess_a2;
  // real mu_sess_w;
  
  // For log likelihood calculation
  real log_lik[N];
  
  // For posterior predictive check
  real y_pred[N,T];
  real y_pred_t0[N,T];
  // real y_pred_step2[N,T];
  
  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i,t] = -1;
      // y_pred_step2[i,t] = -1;
    }
  }
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred_t0[i,t] = -1;
      // y_pred_step2[i,t] = -1;
    }
  }
  // Generate group level parameter values
 
  // real beta_ami_ankk;
  // real beta_nal_dat;
  // real beta_ami_dat;
  // real beta_nal_comt;
  // real beta_ami_comt;
  // real beta_nal_darpp;
  // real beta_ami_darpp;
  // 
  // real beta_ankk;
  // 
  // real beta_dat;
  // 
  // real beta_comt;
  // 
  // real beta_darpp;
  // 
  // 
  
  
  
  { // local section, this saves time and space
  for (i in 1:N) {
    // Define values
    // Define values
    vector[2] v_mb;    // model-based stimulus values for level 1 (2 stimuli)
    matrix[2,2] v_mf;    // model-free stimulus values for level 1&2 (1,2--> level 1, 3-6--> level 2)
    vector[2] v_2;
    vector[2] v_hybrid;  // hybrid stimulus values for level 1 (2 stimuli)
    real level1_prob_choice2; // Initialize prob. of choosing stim 2 (0 or 1) in level 1
    
    matrix[2,2] trans_prob_state1_1;
    matrix[2,2] trans_prob_state1_2;
    int trans_prob_state1_1_changed;
    int trans_prob_state1_2_changed;
    int choice;
    int choice01;
    int action_right;
    int action_left;
    int pressed_left_prev;
    // int level2_choice_01;
    
    real delta;
    real a1_par;
    real a1_g_par;
    real a1_l_par;
    real a2_par;
    real g_par;
    real st_par;
    real w_par;
    
    w_par=w[i];
    a1_g_par = 1;
    a1_l_par = 1;
    a2_par = 1;
    g_par = g[i];
    st_par = 0;
    // Initialize values
    trans_prob_state1_1 = rep_matrix(0.5, 2,2);
    trans_prob_state1_2 = rep_matrix(0.5, 2,2);
    v_mb  = rep_vector(0.0, 2);
    v_mf  = rep_matrix(0.0, 2,2);
    v_2 = rep_vector(0.0,2);
    v_hybrid = rep_vector(0.0, 2);
    trans_prob_state1_1_changed = 0;
    trans_prob_state1_2_changed = 0;
    
    log_lik[i] = 0;
    
    for (t in 1:Tsubj[i])  {
      
      // mark that the agent has learned the state transfer
      if (state1[i,t] == 1 && trans_prob_state1_1_changed== 0) {
        trans_prob_state1_1_changed = 1;
      } else if (state1[i,t] == 2 && trans_prob_state1_2_changed== 0) {
        trans_prob_state1_2_changed = 1;
      }
      
      // compute v_mb 
      //    Qmb = Tm{s1}'*Q2;     
      if (state1[i,t]==1) {
        v_mb = trans_prob_state1_1 * v_2;
      } else if (state1[i,t]==2) {
        v_mb = trans_prob_state1_2 * v_2;
      }
      
      
      
      // compute v_hybrid
      //    Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
      v_hybrid[1] = w_par * v_mb[1] + (1-w_par) * v_mf[state1[i,t],1];   // Q of choosing 1,3 (state 2 = 1) 
      v_hybrid[2] = w_par * v_mb[2] + (1-w_par) * v_mf[state1[i,t],2];   // Q of choosing 2,4 (state 2 = 2)
      
      // set the choice from 1-4 to 1-2
      choice = level1_choice[i,t];
      if(choice > 2){
        choice = choice - 2;
      }
      
      // def choice for bernoulli
      choice01 = choice -1;  
      
      //    
      
      
      
      //  agent realizes transition structure:
      if (trans_prob_state1_1_changed == 1 && state1[i,t] == 1) {
        trans_prob_state1_1 = [[1, 0],[0, 1]];
      }         //  agent realizes transition structure
      
      if (trans_prob_state1_2_changed == 1 && state1[i,t] == 2) {
        trans_prob_state1_2 = [[1, 0],[0 ,1]];
      } 
      
      // make the last choice sticky
      if(!(t == 1)) {
        pressed_left_prev = stim_left[i,t-1] == level1_choice[i,t-1];
        action_left = stim_left[i,t];
        if (action_left > 2) action_left = action_left- 2;
        action_right = 3 - action_left; 
        
        if(pressed_left_prev == 1) {
          v_hybrid[action_left] =  v_hybrid[action_left] + st_par;
        } else {
          v_hybrid[action_right] =  v_hybrid[action_right] + st_par;
        }
        
      }
      
      level1_prob_choice2 = inv_logit( beta1[i]*(v_hybrid[2]-v_hybrid[1]));
      
      log_lik[i] = log_lik[i] + bernoulli_lpmf( choice01 | level1_prob_choice2 );
      
      // generate posterior prediction for current trial
      y_pred[i,t] = bernoulli_rng(level1_prob_choice2);
      
      // log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice | beta1[i]*v_hybrid );
      // alternative model formulation
      // y_pred[i,t] = categorical_logit_rng( beta1[i]*v_hybrid);
      
      // Observe Level2 and update Level1 of the chosen option
      //    dtQ(2) = subdata.points(t) - Q2(s2);                            % prediction error (2nd choice)
      delta = reward[i,t] - v_mf[state1[i,t], choice];
      if (delta >0) {
        a1_par = a1_g_par;
      } else {
        a1_par = a1_l_par;
      }
      
      v_mf[state1[i,t], choice] = v_mf[state1[i,t], choice] + a1_par*delta;
      
      // After observing the reward at Level 2...
      // Update Level 2 v_mf of the chosen option. Level 2--> choose one of level 2 options and observe reward
      //    Q2(s2) = Q2(s2) + lr*dtQ(2);            
      v_2[state2[i,t]] =v_2[state2[i,t]] + a2_par*(reward[i,t] - v_2[state2[i,t]] );
      
      
      // forget others
      
      v_mf[state1[i,t], 3-choice] = (1-g_par)*v_mf[state1[i,t], 3-choice];
      v_mf[3-state1[i,t], choice] = (1-g_par)*v_mf[3-state1[i,t], choice];
      v_mf[3-state1[i,t], 3-choice] = (1-g_par )*v_mf[3-state1[i,t], 3-choice];
      
      // Update Level 1 v_mf with eligibility trace
      //    Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);   
      // v_mf[state1[i,t], choice] = v_mf[state1[i,t], choice] + lambda[i] * a1[i] * (reward[i,t] - v_2[state2[i,t]] );
    } // end of t loop
    
    ////////////////////////////////////// start with t0
    if (Tsubj_t0[i]!= 1) {
      
      
      vector[2] v_mb_t0;    // model-based stimulus values for level 1 (2 stimuli)
      matrix[2,2] v_mf_t0;    // model-free stimulus values for level 1&2 (1,2--> level 1, 3-6--> level 2)
      vector[2] v_2_t0;
      vector[2] v_hybrid_t0;  // hybrid stimulus values for level 1 (2 stimuli)
      real level1_prob_choice2_t0; // Initialize prob. of choosing stim 2 (0 or 1) in level 1
      matrix[2, 2] trans_prob_state1_1_t0;
      matrix[2, 2] trans_prob_state1_2_t0;
      int trans_prob_state1_1_changed_t0;
      int trans_prob_state1_2_changed_t0;
      int choice_t0;
      int choice01_t0;
      int action_right_t0;
      int action_left_t0;
      int pressed_left_prev_t0;
      
      real a1_t0_par;
      real a1_g_t0_par;
      real a1_l_t0_par;
      real a2_t0_par;
      real g_t0_par;
      real st_t0_par;
      real w_t0_par;
      
      w_t0_par = w_t0[i];
      a1_g_t0_par = 1;
      a1_l_t0_par = 1;
      a2_t0_par = 1;
      g_t0_par = g_t0[i];
      st_t0_par= 0 ;
      // Initialize values
      trans_prob_state1_1_t0 = rep_matrix(0.5, 2,2);
      trans_prob_state1_2_t0 = rep_matrix(0.5, 2,2);
      v_mb_t0  = rep_vector(0.0, 2);
      v_mf_t0  = rep_matrix(0.0, 2,2);
      v_2_t0 = rep_vector(0.0,2);
      v_hybrid_t0 = rep_vector(0.0, 2);
      trans_prob_state1_1_changed_t0 = 0;
      trans_prob_state1_2_changed_t0 = 0;
      
      for (t in 1:Tsubj_t0[i])  {
        
        // mark that the agent has learned the state transfer
        if (state1_t0[i,t] == 1 && trans_prob_state1_1_changed_t0== 0) {
          trans_prob_state1_1_changed_t0 = 1;
        } else if (state1_t0[i,t] == 2 && trans_prob_state1_2_changed_t0== 0) {
          trans_prob_state1_2_changed_t0 = 1;
        }
        
        // compute v_mb 
        //    Qmb = Tm{s1}'*Q2;     
        if (state1_t0[i,t]==1) {
          v_mb_t0 = trans_prob_state1_1_t0 * v_2_t0;
        } else if (state1_t0[i,t]==2) {
          v_mb_t0 = trans_prob_state1_2_t0 * v_2_t0;
        }
        
        
        
        // compute v_hybrid
        //    Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
        v_hybrid_t0[1] = w_t0_par * v_mb_t0[1] + (1-w_t0_par) * v_mf_t0[state1_t0[i,t],1];   // Q of choosing 1,3 (state 2 = 1) 
        v_hybrid_t0[2] = w_t0_par * v_mb_t0[2] + (1-w_t0_par) * v_mf_t0[state1_t0[i,t],2];   // Q of choosing 2,4 (state 2 = 2)
        
        // set the choice from 1-4 to 1-2
        choice_t0 = level1_choice_t0[i,t];
        if(choice_t0 > 2){
          choice_t0 = choice_t0 - 2;
        }
        
        // def choice for bernoulli
        choice01_t0 = choice_t0 -1;  
        
        //    
        
        
        
        //  agent realizes transition structure:
        if (trans_prob_state1_1_changed_t0 == 1 && state1_t0[i,t] == 1) {
          trans_prob_state1_1_t0 = [[1, 0],[0, 1]];
        }         //  agent realizes transition structure
        
        if (trans_prob_state1_2_changed_t0 == 1 && state1_t0[i,t] == 2) {
          trans_prob_state1_2_t0 = [[1, 0],[0, 1]];
        } 
        
        // make the last choice sticky
        if(!(t == 1)) {
          pressed_left_prev_t0 = stim_left_t0[i,t-1] == level1_choice_t0[i,t-1];
          action_left_t0 = stim_left_t0[i,t];
          if (action_left_t0 > 2) action_left_t0 = action_left_t0- 2;
          action_right_t0 = 3 - action_left_t0; 
          
          if(pressed_left_prev_t0 == 1) {
            v_hybrid_t0[action_left_t0] =  v_hybrid_t0[action_left_t0] + st_t0_par;
          } else {
            v_hybrid_t0[action_right_t0] =  v_hybrid_t0[action_right_t0] + st_t0_par;
          }
          
        }
        
        level1_prob_choice2_t0 = inv_logit( beta1_t0[i]*(v_hybrid_t0[2]-v_hybrid_t0[1]));
        
        // alternative model formulation
        
        
        log_lik[i] = log_lik[i] + bernoulli_lpmf( choice01_t0 | level1_prob_choice2_t0);
        // log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice_t0 | beta1_t0[i]*v_hybrid_t0 );
        
        // generate posterior prediction for current trial
        y_pred_t0[i,t] = bernoulli_rng(level1_prob_choice2_t0);
        // y_pred_t0[i,t] = categorical_logit_rng( beta1_t0[i]*v_hybrid_t0);
        
        
        // alternative model formulation
        // choice ~ categorical_logit( beta1[i]*(v_hybrid[2]-v_hybrid[1]))
        
        // Observe Level2 and update Level1 of the chosen option
        //    dtQ(2) = subdata.points(t) - Q2(s2);                            % prediction error (2nd choice)
        delta = reward_t0[i,t]  - v_mf_t0[state1_t0[i,t], choice_t0];
        if (delta >0) {
          a1_t0_par = a1_g_t0_par;
        } else {
          a1_t0_par = a1_l_t0_par;
        }
        
        v_mf_t0[state1_t0[i,t], choice_t0] = v_mf_t0[state1_t0[i,t], choice_t0] + a1_t0_par*delta;
        
        // After observing the reward at Level 2...
        // Update Level 2 v_mf of the chosen option. Level 2--> choose one of level 2 options and observe reward
        //    Q2(s2) = Q2(s2) + lr*dtQ(2);            
        v_2_t0[state2_t0[i,t]] =v_2_t0[state2_t0[i,t]] + a2_t0_par*(reward_t0[i,t] - v_2_t0[state2_t0[i,t]] );
        
        // Update Level 1 v_mf with eligibility trace
        //    Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);   
        // v_mf_t0[state1_t0[i,t], choice_t0] = v_mf_t0[state1_t0[i,t], choice_t0] + lambda_t0[i] * a1_t0[i] * (reward_t0[i,t] - v_2_t0[state2_t0[i,t]] );
        
        // forget others
        
        v_mf_t0[state1_t0[i,t], 3-choice_t0] = (1-g_t0_par)*v_mf_t0[state1_t0[i,t], 3-choice_t0];
        v_mf_t0[3-state1_t0[i,t], choice_t0] = (1-g_t0_par)*v_mf_t0[3-state1_t0[i,t], choice_t0];
        v_mf_t0[3-state1_t0[i,t], 3-choice_t0] = (1-g_t0_par)*v_mf_t0[3-state1_t0[i,t], 3-choice_t0];
        
      } // end of t loop
      
      
      
      
      
    } // end of if Tsubj_t0[i] != 1;
    ////////////////////////////////////////// end of t0
    
  } // end of i loop
  } // end local
}

