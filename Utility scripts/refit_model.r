
modelfile <- "StanScripts/ts_g_wT0_lkj.stan"
savemodelname <- "Model_results/Final_models/M_g_lkj_new.rds"
savedataset <- "Refit_winning_model" #without .rds 
fit_model <- readRDS(paste("~/mnt/p/userdata/mikusn22/data/2step_task/AnalysisR/", savemodelname, sep = ""))
library(Stack)
library(rstan)
library(ggplot2)
# library(R.matlab)
library(tidyr)
library(dplyr)
remove_missin_t0s = 1
use_genetics = 0 
no_subjects = NA
# par_set_gen <- rstan::extract(fit_model, pars = c("w_mean", "beta1_mean", "g_mean","sess_w", "sess_beta1",  "sess_g",
                                               # "w", "beta1", "g", "w_t0", "beta1_t0", "g_t0"))
# random_samples = c(2200, 4456,5987, 6509, 7890)

# hist(par_set_gen$w, 50)
no_of_samples <- 1
# data_pred =  matrix( nrow=0 , ncol=11 )


source("Model3_stimulations.r")

### load data


for (sim in 1: no_of_samples) {
  # no_of_samples= dim(y_pred)[1]
  cat("running simulation ", sim, "\n")
  choice01 <- y_pred[sim,,]
  choice01_t0 <- y_pred_t0[sim,,]
  
  reward <- y_reward[sim,,]
  reward_t0 <- y_reward_t0[sim,,]
  
  # View(reward)
  
  data_id_wow <- data_beh[data_beh$s == 26,]
  state1 <- data_id_wow$s1[data_id_wow$session==2]
  
  state1_t0 <- data_id_wow$s1[data_id_wow$session==1]
  
  
  Tsubj <- as.vector(rep(0, numSub))
  for (ss in 1:numSub) {
    Tsubj[ss] <- 200;
  }
  
  Tsubj_t0 <- as.vector(rep(0, numSub))
  for (ss in 1:numSub) {
    Tsubj_t0[ss] <- 200;
    if (Tsubj_t0[ss]==-1) {
      Tsubj_t0[ss] =1
    }
  }
  maxTrials <- 200 # max(Tsubj)
  
  level1_choice <- array(1,c(numSub, maxTrials))
  stim_left <- array(1,c(numSub, maxTrials))
  # reward <- array(1,c(numSub, maxTrials))
  state1 <- array(1,c(numSub, maxTrials))
  state2 <-array(1,c(numSub, maxTrials))
  level1_choice_t0 <- array(1,c(numSub, maxTrials))
  stim_left_t0 <- array(1,c(numSub, maxTrials))
  # reward_t0 <- array(1,c(numSub, maxTrials))
  state1_t0 <- array(1,c(numSub, maxTrials))
  state2_t0 <-array(1,c(numSub, maxTrials))
  
  for (i in 1:numSub) {
    # tmp <- subset(data_temp, data_temp$s==subjList[i])
    # level1_choice[i,1:Tsubj[i]] = tmp$choice
    # stim_left[i,1:Tsubj[i]] = tmp$stim_left
    # reward[i,1:Tsubj[i]] = tmp$points
    state1[i,1:Tsubj[i]] =  data_id_wow$s1[data_id_wow$session==2]
    state2[i,1:Tsubj[i]] =  y_pred[sim,i,] + 1
    # tmp <- subset(data_temp_t0, data_temp_t0$s==subjList[i])
    
    # stim_left_t0[i,1:Tsubj_t0[i]] = tmp$stim_left
    # reward_t0[i,1:Tsubj_t0[i]] = tmp$points
    state1_t0[i,1:Tsubj_t0[i]] = data_id_wow$s1[data_id_wow$session==1]
    state2_t0[i,1:Tsubj_t0[i]] =  y_pred_t0[sim,i,] + 1
    
    
    level1_choice[i,1:Tsubj[i]] = y_pred[sim,i,] + 1 + 2*(state1[i,1:Tsubj[i]] - 1)
    level1_choice_t0[i,1:Tsubj_t0[i]] = y_pred_t0[sim,i,] + 1 + 2*(state1_t0[i,1:Tsubj_t0[i]] - 1)
  }
  
  
  
  
  
  
  
  
  subjList_t0<- subjList
  
  cat("running on ", length(subjList), " subjects out of which ", length(subjList_t0), " have also t0 data\n")
  # sum(comt==2, na.rm = T)
  #
  # comt <- (as.numeric(comt==3))
  # 
  # subjList <- unique(data_temp$s)
  numSub <- length(subjList)
  
  
  # serum[amisulpride==1]
  dataList <- list(N = numSub, T = maxTrials, Tsubj = Tsubj, Tsubj_t0=Tsubj_t0, level1_choice = level1_choice, stim_left = stim_left,
                   reward = reward, state1 = state1, state2 = state2,
                   level1_choice_t0 = level1_choice_t0,stim_left_t0 = stim_left_t0,
                   reward_t0 = reward_t0, state1_t0 = state1_t0, state2_t0 = state2_t0)
  #                 simulate_data = simulate_data, prior_mu = prior_mu, prior_sd  = prior_sd, prior_sd_random=prior_sd_random)
  
  
  # =============================================================================
  #### Running Stan #### 
  # =============================================================================
  rstan_options(auto_write = TRUE)
  options(mc.cores = 4)
  
  
  nIter     <- 2000
  nChains   <- 4
  nWarmup   <- 1000 # floor(nIter/2)
  nThin     <- 1
  
  cat("Estimating", modelfile, "model... \n")
  
  startTime = Sys.time(); print(startTime)
  
  modelcode <-  stan_model(modelfile)
  
  fit_rl <-  sampling(modelcode, 
                      data = dataList,
                      chains = nChains,
                      iter = nIter,
                      warmup = nWarmup,
                      thin = nThin,
                      init = "random",
                      seed = 1450154637,
                      control = list(adapt_delta = 0.9, max_treedepth=10)
  )
  # par_set_gen <- rstan::extract(fit_rl, pars = c("w_mean", "beta1_mean", "g_mean","sess_w", "sess_beta1",  "sess_g"))
  
  
  
  drf_temp <- tibble(
    
    beta1_mean_rstan = data_group$beta1_mean_rstan,
    w_mean_rstan= data_group$w_mean_rstan,
    g_mean_rstan = data_group$g_mean_rstan,
    
    sess_beta1_rstan = data_group$sess_beta1_rstan,
    sess_w_rstan = data_group$sess_w_rstan,
    sess_g_rstan = data_group$sess_g_rstan,
    
    beta1_raw_rstan =   data_group$beta1_mean_rstan+ data_group$sess_beta1_rstan,
    w_raw_rstan =  data_group$w_mean_rstan + data_group$sess_w_rstan ,
    g_raw_rstan =  data_group$g_mean_rstan + data_group$sess_g_rstan,
    
    
    beta1_mean_rstan_rf = get_posterior_mean(fit_rl, pars=c('beta1_mean'))[,5],
    w_mean_rstan_rf = get_posterior_mean(fit_rl, pars=c('w_mean'))[,5],
    g_mean_rstan_rf = get_posterior_mean(fit_rl, pars=c('g_mean'))[,5],
    
    sess_beta1_rstan_rf=  get_posterior_mean(fit_rl, pars=c('sess_beta1'))[,5],
    sess_w_rstan_rf = get_posterior_mean(fit_rl, pars=c('sess_w'))[,5],
    sess_g_rstan_rf = get_posterior_mean(fit_rl, pars=c('sess_g'))[,5],
    
    beta1_raw_rstan_rf =   beta1_mean_rstan_rf+ sess_beta1_rstan_rf,
    w_raw_rstan_rf =  w_mean_rstan_rf + sess_w_rstan_rf ,
    g_raw_rstan_rf =  g_mean_rstan_rf + sess_g_rstan_rf,
    
    sample_no = sim
  )
  if (sim == 1) {
    drf = drf_temp 
  } else {drf = rbind(drf, drf_temp)}
  
  

  
  
  
  cat("Finishing", modelfile, "model simulation ... \n")
  endTime = Sys.time(); print(endTime)
  cat("It took",as.character.Date(endTime - startTime), "\n")
  cat("Saving in ", savedataset, "... \n")
  saveRDS(fit_rl, file = paste(savedataset, "_sample_no_", sim, ".rds", sep = ""))
  # saveRDS(mu_p_mats_subset, file = paste(savemodelname, "gen_params.rds"))
  # return(fit_rl)
  
  
}

saveRDS(drf, file = paste(savedataset, "_pars_all.rds", sep = ""))

