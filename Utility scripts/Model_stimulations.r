# data 

subjList = data_group$s %>% unique()
# load model parameters ----------------
  data_group$beta1_mean_rstan <- get_posterior_mean(fit_model, pars=c('beta1_mean'))[,5]
  data_group$w_mean_rstan <- get_posterior_mean(fit_model, pars=c('w_mean'))[,5]
  data_group$g_mean_rstan <- get_posterior_mean(fit_model, pars=c('g_mean'))[,5]
  
 
  data_group$beta1_t0_rstan <- get_posterior_mean(fit_model, pars=c('beta1_t0'))[,5]
  data_group$w_t0_rstan <- get_posterior_mean(fit_model, pars=c('w_t0'))[,5]
  data_group$g_t0_rstan <- get_posterior_mean(fit_model, pars=c('g_t0'))[,5]
  
  data_group$beta1_rstan <- get_posterior_mean(fit_model, pars=c('beta1'))[,5]
  data_group$w_rstan <- get_posterior_mean(fit_model, pars=c('w'))[,5]
  data_group$g_rstan <- get_posterior_mean(fit_model, pars=c('g'))[,5]
 
  data_group$sess_beta1_rstan <- get_posterior_mean(fit_model, pars=c('sess_beta1'))[,5]
  data_group$sess_w_rstan <- get_posterior_mean(fit_model, pars=c('sess_w'))[,5]
  data_group$sess_g_rstan <- get_posterior_mean(fit_model, pars=c('sess_g'))[,5]
 
  data_group$beta1_raw_rstan <-   data_group$beta1_mean_rstan+ data_group$sess_beta1_rstan
  data_group$w_raw_rstan <-  data_group$w_mean_rstan + data_group$sess_w_rstan 
  data_group$g_raw_rstan <-  data_group$g_mean_rstan + data_group$sess_g_rstan
# make predictions ----------
  

y_pred = array(rep(-1,no_of_samples*length(subjList)*200), dim=c(no_of_samples,length(subjList),200))
y_pred_t0 = array(rep(-1,no_of_samples*length(subjList)*200), dim=c(no_of_samples,length(subjList),200))
y_reward = array(rep(-1,no_of_samples*length(subjList)*200), dim=c(no_of_samples,length(subjList),200))
y_reward_t0 = array(rep(-1,no_of_samples*length(subjList)*200), dim=c(no_of_samples,length(subjList),200))



data_id_wow <- data_beh[data_beh$s == 26,]
state1 <- data_id_wow$s1[data_id_wow$session==2]
state1_t0 <- data_id_wow$s1[data_id_wow$session==1]
reward <- cbind(data_id_wow$rew_stim1[data_id_wow$session==2],data_id_wow$rew_stim2[data_id_wow$session==2])
reward_t0 <- cbind(data_id_wow$rew_stim1[data_id_wow$session==1],data_id_wow$rew_stim2[data_id_wow$session==1])
for (s in 1:no_of_samples) {
  for (i in 1:length(subjList)) { #
    
    w_t0_par <- data_group$w_t0_rstan[i]
    w_par <- data_group$w_rstan[i]
    
    g_t0_par <- data_group$g_t0_rstan[i]
    g_par <- data_group$g_rstan[i]
    
    beta1_t0 <- data_group$beta1_t0_rstan[i]
    beta1 <- data_group$beta1_rstan[i]
    
    # data_group%>% ggplot(aes(x=drug, y = )) + geom_point()
    
    
    trans_prob_state1_1 = matrix(0.5, 2,2);
    trans_prob_state1_2 = matrix(0.5, 2,2);
    v_mb  = matrix(0.0,2,1);
    v_mf  = matrix(0.0, 2,2);
    v_2 = matrix(0.0,2,1);
    v_hybrid = matrix(0.0, 2);
    trans_prob_state1_1_changed = 0;
    trans_prob_state1_2_changed = 0;
    
    for (t in 1:200)  {
      
      # mark that the agent has learned the state transfer
      if (state1[t] == 1 && trans_prob_state1_1_changed== 0) {
        trans_prob_state1_1_changed = 1;
      } else if (state1[t] == 2 && trans_prob_state1_2_changed== 0) {
        trans_prob_state1_2_changed = 1;
      }
      
      # compute v_mb 
      #    Qmb = Tm{s1}'*Q2;     
      if (state1[t]==1) {
        v_mb = trans_prob_state1_1 %*% v_2;
      } else if (state1[t]==2) {
        v_mb = trans_prob_state1_2  %*% v_2;
      }
      
      
      
      # compute v_hybrid
      #    Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
      v_hybrid[1] = w_par * v_mb[1] + (1-w_par) * v_mf[state1[t],1];   # Q of choosing 1,3 (state 2 = 1) 
      v_hybrid[2] = w_par * v_mb[2] + (1-w_par) * v_mf[state1[t],2];   # Q of choosing 2,4 (state 2 = 2)
      
      # def choice for bernoulli
      # choice01 = choice -1;  
      # 
      #    
      
      
      
      #  agent realizes transition structure:
      if (trans_prob_state1_1_changed == 1 && state1[t] == 1) {
        trans_prob_state1_1 =matrix(c(1,0,0,1 ) ,2,2)
      }         #  agent realizes transition structure
      
      if (trans_prob_state1_2_changed == 1 && state1[t] == 2) {
        trans_prob_state1_2=matrix(c(1,0,0,1 ) ,2,2)
      } 
      
      # make the last response sticky
      
      choice01 <- rbinom(1,1, inv_logit( beta1*(v_hybrid[2]-v_hybrid[1])))  # level 1, prob. of choosing 2 in level 1
      y_pred[s,i,t] <- choice01
      
      choice = choice01 +1
      # Observe Level2 and update Level1 of the chosen option
      
      v_mf[state1[t], choice] = reward[t,choice] 
      y_reward[s,i,t] = reward[t,choice]
      # After observing the reward at Level 2...
      # Update Level 2 v_mf of the chosen option. Level 2--> choose one of level 2 options and observe reward
      #    Q2(s2) = Q2(s2) + lr*dtQ(2);            
      v_2[choice] =reward[t,choice] 
      
      # Update Level 1 v_mf with eligibility trace
      #    Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);   
      # v_mf[state1[t], choice] = v_mf[state1[t], choice] + lambda[i] * a1[i] * (reward[t] - v_2[state2[t]] );
      # forget others
      
      v_mf[state1[t], 3-choice] = (1-g_par)*v_mf[state1[t], 3-choice];
      v_mf[3-state1[t], choice] = (1-g_par)*v_mf[3-state1[t], choice];
      v_mf[3-state1[t], 3-choice] = (1-g_par )*v_mf[3-state1[t], 3-choice];
      
      
    } # end of t loop
    
    ################### start with t0
    
    # Initialize values
    trans_prob_state1_1_t0 = matrix(0.5, 2,2);
    trans_prob_state1_2_t0 = matrix(0.5, 2,2);
    v_mb_t0  = matrix(0.0, 2);
    v_mf_t0  = matrix(0.0, 2,2);
    v_2_t0 = matrix(0.0,2);
    v_hybrid_t0 = matrix(0.0, 2);
    trans_prob_state1_1_changed_t0 = 0;
    trans_prob_state1_2_changed_t0 = 0;
    
    for (t in 1:200)  {
      
      # mark that the agent has learned the state transfer
      if (state1_t0[t] == 1 && trans_prob_state1_1_changed_t0== 0) {
        trans_prob_state1_1_changed_t0 = 1;
      } else if (state1_t0[t] == 2 && trans_prob_state1_2_changed_t0== 0) {
        trans_prob_state1_2_changed_t0 = 1;
      }
      
      # compute v_mb 
      #    Qmb = Tm{s1}'*Q2;     
      if (state1_t0[t]==1) {
        v_mb_t0 = trans_prob_state1_1_t0 %*% v_2_t0;
      } else if (state1_t0[t]==2) {
        v_mb_t0 = trans_prob_state1_2_t0 %*% v_2_t0;
      }
      
      
      
      # compute v_hybrid
      #    Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
      v_hybrid_t0[1] = w_t0_par * v_mb_t0[1] + (1-w_t0_par) * v_mf_t0[state1_t0[t],1];   # Q of choosing 1,3 (state 2 = 1) 
      v_hybrid_t0[2] = w_t0_par * v_mb_t0[2] + (1-w_t0_par) * v_mf_t0[state1_t0[t],2];   # Q of choosing 2,4 (state 2 = 2)
      
      
      
      
      #  agent realizes transition structure:
      if (trans_prob_state1_1_changed_t0 == 1 && state1_t0[t] == 1) {
        trans_prob_state1_1_t0 = matrix(c(1,0,0,1 ),2,2)
      }         #  agent realizes transition structure
      
      if (trans_prob_state1_2_changed_t0 == 1 && state1_t0[t] == 2) {
        trans_prob_state1_2_t0 =matrix(c(1,0,0,1 ), 2,2)
      } 
      
      # make the last choice sticky
      
      
      choice01 <- rbinom(1,1, inv_logit( beta1_t0*(v_hybrid_t0[2]-v_hybrid_t0[1])))  # level 1, prob. of choosing 2 in level 1
      y_pred_t0[s,i,t] = choice01
      choice_t0 = choice01 +1 
      # alternative model formulation
      # choice_t0 ~ categorical_logit( beta1_t0[i]*v_hybrid_t0);
      
      # Observe Level2 and update Level1 of the chosen option
      #    dtQ(2) = subdata.points(t) - Q2(s2);                            % prediction error (2nd choice)
      
      v_mf_t0[state1_t0[t], choice_t0] =reward_t0[t,choice_t0]
      y_reward_t0[s,i,t] = reward_t0[t,choice_t0]
      # After observing the reward at Level 2...
      # Update Level 2 v_mf of the chosen option. Level 2--> choose one of level 2 options and observe reward
      #    Q2(s2) = Q2(s2) + lr*dtQ(2);            
      v_2_t0[choice_t0] =reward_t0[t,choice_t0]
      
      # forget others
      
      v_mf_t0[state1_t0[t], 3-choice_t0] = (1-g_t0_par)*v_mf_t0[state1_t0[t], 3-choice_t0];
      v_mf_t0[3-state1_t0[t], choice_t0] = (1-g_t0_par)*v_mf_t0[3-state1_t0[t], choice_t0];
      v_mf_t0[3-state1_t0[t], 3-choice_t0] = (1-g_t0_par)*v_mf_t0[3-state1_t0[t], 3-choice_t0];
      
    
      
    } # end of t loop
    ##################### end of t0
    
    # PredChoice01 = as.vector(t(data_id_t0)),
    # PredPrevChoice01 = lag(as.vector(t(data_id_t0))),
    # PredStay = as.vector(t(data_id_t0)) == lag(as.vector(t(data_id_t0))),
    # sim = as.vector(t(matrix(rep(1: dim(data_id_t0)[1], dim(data_id_t0)[2]),dim(data_id_t0)[1]))),
    # count_sim = (l-1)*no_of_samples + as.vector(t(matrix(rep(1: dim(data_id_t0)[1], dim(data_id_t0)[2]),dim(data_id_t0)[1]))),
    # prev_points = rep(data_id_wow$prev_points[data_id_wow$session == 1],dim(data_id_t0)[1]),
    # prev_state_diff = rep(data_id_wow$prev_state_diff[data_id_wow$session == 1],dim(data_id_t0)[1]),
    # trial =rep(1:dim(data_id_t0)[2],dim(data_id_t0)[1]),
    # s= subjList[l],
    # drug = drug_id_t0,
    # session = 1),
    
    
    # Set all posterior predictions to 0 (avoids NULL values)
    
    
    
  } # end of i loop
}

