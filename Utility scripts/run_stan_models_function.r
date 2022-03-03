# =============================================================================
#### Info #### 
# =============================================================================
# hierarchical model 
# adapted by Nace Mikus from a script by Lei Zhang (hBayesDM package)

run_model_fit <- function(modelfile, savemodelname, InfType = "Sampling") {
  
  # =============================================================================
  #### Construct Data #### 
  # =============================================================================

 
  library(rstan)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  use_genetics = 0
 
   ### load data
  data_temp <- readRDS("Data/data_beh.rds")
  comt <- data_temp$comt[data_temp$trials==9]
  data_temp_t0 <- data_temp %>% filter(session == 1)
  data_temp <- data_temp %>% filter(session == 2)
  
  if (use_genetics == 1) {
    ExcludeSubj <- subjList[is.na(comt)]
    subjList <- setdiff(subjList, ExcludeSubj)
    
    ### remove subjects with no genetic data
    data_temp <- data_temp[data_temp$s %in% subjList,  ]
    data_temp_t0 <- data_temp_t0[data_temp_t0$s %in% subjList,  ]
    print("removed sub with no genetic data")
  }
  
  ankk <- data_temp$ankk[data_temp$trials==9]
  darpp <- data_temp$darpp[data_temp$trials==9]
  ankk <-  as.numeric(ankk == "a1+")
  darpp<-  as.numeric(darpp == "TT")
  
  subjList <- unique(data_temp$s)
  subjList_t0<- unique(data_temp_t0$s[data_temp_t0$trials != -1])
  
  # center genetic variables
  dat<- data_temp$dat[data_temp$trials==9]
  dat <- (as.numeric(dat=="10/10"))
  dat[dat ==1] <- 0.5
  dat[dat ==0] <- -0.5
  ankk[ankk ==1] <- 0.5
  ankk[ankk ==0] <- -0.5
  darpp[darpp ==1] <- 0.5
  darpp[darpp ==0] <- -0.5
  
  comt <- data_temp$comt[data_temp$trials==9]
  comt <- (as.numeric(comt))
  comt <- (comt - mean(comt))/sd(comt)
  
  
  
  # comt <- (as.numeric(comt==3))
  # 
  # subjList <- unique(data_temp$s)
  numSub <- length(subjList)
  
  
  
  
  Tsubj <- as.vector(rep(0, numSub))
  for (ss in 1:numSub) {
    Tsubj[ss] <- length(data_temp$trials[data_temp$s == subjList[ss]]);
  }
  
  Tsubj_t0 <- as.vector(rep(0, numSub))
  for (ss in 1:numSub) {
    Tsubj_t0[ss] <- length(data_temp_t0$trials[data_temp_t0$s == subjList[ss]]);
    if (Tsubj_t0[ss]==-1) {
      Tsubj_t0[ss] =1
    }
  }
  maxTrials <- 200 # max(Tsubj)
  
  level1_choice <- array(1,c(numSub, maxTrials))
  stim_left <- array(1,c(numSub, maxTrials))
  reward <- array(1,c(numSub, maxTrials))
  state1 <- array(1,c(numSub, maxTrials))
  state2 <-array(1,c(numSub, maxTrials))
  level1_choice_t0 <- array(1,c(numSub, maxTrials))
  stim_left_t0 <- array(1,c(numSub, maxTrials))
  reward_t0 <- array(1,c(numSub, maxTrials))
  state1_t0 <- array(1,c(numSub, maxTrials))
  state2_t0 <-array(1,c(numSub, maxTrials))
  
  for (i in 1:numSub) {
    tmp <- subset(data_temp, data_temp$s==subjList[i])
    level1_choice[i,1:Tsubj[i]] = tmp$choice
    stim_left[i,1:Tsubj[i]] = tmp$stim_left
    reward[i,1:Tsubj[i]] = tmp$points
    state1[i,1:Tsubj[i]] = tmp$s1
    state2[i,1:Tsubj[i]] = tmp$s2
    tmp <- subset(data_temp_t0, data_temp_t0$s==subjList[i])
    level1_choice_t0[i,1:Tsubj_t0[i]] = tmp$choice
    stim_left_t0[i,1:Tsubj_t0[i]] = tmp$stim_left
    reward_t0[i,1:Tsubj_t0[i]] = tmp$points
    state1_t0[i,1:Tsubj_t0[i]] = tmp$s1
    state2_t0[i,1:Tsubj_t0[i]] = tmp$s2
  }
  
  
  
  data_temp_drug <- data_temp %>% 
    group_by(s) %>%
    summarize(drug = drug[1])
  # drug <-factor(data_temp_drug$drug, level = c(1,2,3), labels = c("ami", "nal", "pla"))
  naltrexone <- data_temp_drug$drug == "Nal" # Nal
  naltrexone<- as.numeric(naltrexone)
  # length(amisulpride)
  amisulpride <- data_temp_drug$drug == "Ami" # amisul
  amisulpride<- as.numeric(amisulpride)
  
  dataList <- list(N = numSub, T = maxTrials, Tsubj = Tsubj, Tsubj_t0=Tsubj_t0, level1_choice = level1_choice, stim_left = stim_left,
                   reward = reward, state1 = state1, state2 = state2,
                   level1_choice_t0 = level1_choice_t0,stim_left_t0 = stim_left_t0,
                   reward_t0 = reward_t0, state1_t0 = state1_t0, state2_t0 = state2_t0,
                   naltrexone= naltrexone, amisulpride= amisulpride,
                   comt = comt, dat=dat, ankk =ankk, darpp = darpp)
  
  # =============================================================================
  #### Running Stan #### 
  # =============================================================================
  rstan_options(auto_write = TRUE)
  options(mc.cores = 4)
  
  if (InfType == "try") {
    nIter     <- 2
    nChains   <- 1
    nWarmup   <- 1 # floor(nIter/2)
    nThin     <- 1
    InfType = "Sampling"
  } else {
    nIter     <- 3000
    nChains   <- 4
    nWarmup   <- 1000 # floor(nIter/2)
    nThin     <- 1
  }
  cat("Estimating", modelfile, "model... \n")
  
  startTime = Sys.time(); print(startTime)
  
  modelcode <-  stan_model(modelfile)
  
  if (InfType == "VB") {
    print('Using Variational Bayesian Inference')
    
    
    
    fit_rl <- vb(modelcode, 
                 data    = dataList, 
                 iter =  50000,
                 init    = "0",
                 seed    = 1450154637,
                 tol_rel_obj = 0.01
    )
    
  } else if (InfType == "Optim") {
    print('Optimizing for point estimation only')
    fit_rl <- optimizing(modelcode, 
                         data    = dataList,
                         init    = "random",
                         verbose = TRUE
    )
    
  } else if (InfType == "Sampling") {# sampling (by  default) 
    cat("Calling", nChains, "simulations in Stan... \n")
    
    fit_rl <-  sampling(modelcode, 
                        data = dataList,
                        chains = nChains,
                        iter = nIter,
                        warmup = nWarmup,
                        thin = nThin,
                        init = "0",
                        seed = 1450154637,
                        control = list(adapt_delta = 0.9, max_treedepth=10)
    )
  }
 
  cat("Finishing", modelfile, "model simulation ... \n")
  endTime = Sys.time(); print(endTime)
  cat("It took",as.character.Date(endTime - startTime), "\n")
  cat("Saving in ", savemodelname, "... \n")
  saveRDS(fit_rl, file = savemodelname)
  
  
}


