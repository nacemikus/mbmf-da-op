
## Effects of dopamine D2 and opioid receptor antagonism on model-based behaviour in healthy volunteers ----
# Analysis script, 
# Nace Mikus | nace.mikus@univie.ac.at, nace.mikus@cas.au.dk

# load packages -----------------------------------------------------------


# library(dplyr)
# library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggthemes)
library(nlme)
library(brms)
# library(tidyverse)  # ggplot, dplyr, and friends
library(broom)
library(loo)
library(rstanarm)
library(rstan)
library(patchwork)  # Lay out multiple ggplot plots; install from https://github.com/thomasp85/patchwork
library(lme4)
library(lmerTest)
library("bayesplot")
# aux functions ----

source("Utility scripts/plotting_functions.r") 
source("Utility scripts/theme_functions_MB.r")
source("Utility scripts/aux_functions.r")

# the aux script for running the models in stan, 

source("Utility scripts/run_stan_models_function.r")

# note that these take long to run, 
# an already estimated model can be sent upon inquiry (nace.mikus@univie.ac.at)

# load data ---------------------------------------------------------------


data_group<- readRDS("/Data/data_group.rds")
data_beh <- readRDS("/Data/data_beh.rds")

rstan_options(auto_write = TRUE)
options(mc.cores = 4)
# 

############################################################################################################
# Behavioral analysis ######################################################################################  

## stats from the brm model ----------------------------------------------

# run model
if (FALSE) { # this might take long to run
  if(!file.exists("Brms_stay_beh_full.rds")){
    fit_model_beh <- brm(stay_num|trials(1) ~ (session*prev_state_diff*prev_points|ID) + session*prev_points*prev_state_diff*ami_dummy+ 
                           session*prev_points*prev_state_diff*nal_dummy,
                         data = data_beh, family = binomial(),
                         prior = c(set_prior("cauchy(0,2)", class = "sd"),
                                   set_prior("normal(0,3)", class = "b"),
                                   set_prior("lkj(2)", class = "cor")),
                         #set_prior("lkj(2)", class = "cor")),
                         warmup = 1, iter =2, chains =1,
                         control = list(adapt_delta = 0.80))
    saveRDS(fit_model_beh, file = "Brms_stay_beh_full.rds")
    fit_model_beh %>% fixef() %>% round(2) %>%  write.csv(file = "Beh_model_results.csv")
  }else {
   
    fit_model_beh <- readRDS("Brms_stay_beh_full.rds")
 
   
    
  }
}

# playaround  to double check----- 
fit_model_beh <- readRDS("Brms_stay_beh_rew_level.rds")
post_sam <- posterior_samples(fit_model_beh, pars = c("^b_", "sd_", "sigma"))
(post_sam$`b_session2:reward_level_highTRUE:prev_state_diffdifferent:ami_dummyAmi`  +
    post_sam$`b_session2:reward_level_highTRUE:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

# diff-same  rew
(post_sam$`b_session2:reward_level_highTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_midhighTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_midlowTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_lowTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

# same  rew
(  post_sam$`b_session2:reward_level_highTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

(  post_sam$`b_session2:reward_level_midhighTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

(  post_sam$`b_session2:reward_level_midlowTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

(  post_sam$`b_session2:reward_level_lowTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

# diff  rew
(post_sam$`b_session2:reward_level_highTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` + post_sam$`b_session2:reward_level_highTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_midhighTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` + post_sam$`b_session2:reward_level_midhighTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` +post_sam$`b_session2:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_midlowTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` + post_sam$`b_session2:reward_level_midlowTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_lowTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` +  post_sam$`b_session2:reward_level_lowTRUE:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`) %>% sf()

##




(post_sam$`b_session2:reward_level_midhighTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_midlowTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:reward_level_lowTRUE:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()



    
(post_sam$`b_session2:reward_level_highTRUE:ami_dummyAmi`) %>% sf()
  
(post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()


(post_sam$`b_session2:reward_level_midhighTRUE:prev_state_diffdifferent:ami_dummyAmi`  +
    post_sam$`b_session2:reward_level_midhighTRUE:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

(post_sam$`b_session2:reward_level_highTRUE:prev_state_diffdifferent:ami_dummyAmi`  +
    post_sam$`b_session2:reward_level_highTRUE:ami_dummyAmi` +
    post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi` +
    post_sam$`b_session2:ami_dummyAmi`)%>% sf()

# prev win loss: ####
fit_model_beh3 <- readRDS("Brms_stay_beh_winloss_full.rds")

# fit_model_beh3 <- readRDS("Brms_points_prevwin_beh_serum.rds")

post_sam <- posterior_samples(fit_model_beh3, pars = c("^b_", "sd_", "sigma"))


post_sam$`b_session2:prevwinwin:prev_state_diffdifferent:ami_dummyAmi` %>% sf()

(post_sam$`b_session2:prevwinwin:prev_state_diffdifferent:ami_dummyAmi`+ post_sam$`b_session2:prevwinwin:prev_state_diffdifferent:serum_ami_high`) %>% sf()


(post_sam$`b_session2:prevwinwin:prev_state_diffdifferent:ami_dummyAmi` + post_sam$`b_session2:prevwinwin:ami_dummyAmi`)%>% sf()

(post_sam$`b_session2:prevwinwin:ami_dummyAmi`)%>% sf()

(post_sam$`b_session2:prevwinwin:ami_dummyAmi` + post_sam$`b_session2:prevwinwin:serum_ami_high`)%>% sf()

#loss only
(post_sam$`b_session2:ami_dummyAmi`)%>% sf()
(post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`)%>% sf()

# win only diff
(post_sam$`b_session2:ami_dummyAmi` +post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`+ post_sam$`b_session2:prevwinwin:prev_state_diffdifferent:ami_dummyAmi` + post_sam$`b_session2:prevwinwin:ami_dummyAmi`)%>% sf()

# win only same
(post_sam$`b_session2:ami_dummyAmi` + post_sam$`b_session2:prevwinwin:ami_dummyAmi`)%>% sf()

(post_sam$`b_session2:ami_dummyAmi` + post_sam$`b_session2:prevwinwin:ami_dummyAmi`+
post_sam$`b_session2:serum_ami_high` + post_sam$`b_session2:prevwinwin:serum_ami_high`)%>% sf()






## prev pts points #####
# fit_model_beh3 <- readRDS("Brms_points_beh_prevpts_full.rds")
fit_model_beh3 <- readRDS("Brms_points_prevpoints_beh_serum.rds")
post_sam <- posterior_samples(fit_model_beh3, pars = c("^b_", "sd_", "sigma"))
fit_model_beh3 %>% mcmc_plot( pars = c("^b_"))
rew_size  = 5

# diff- same rew 5

(rew_size*post_sam$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`+ post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`) %>% sf

# high serum
(post_sam$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi` + post_sam$`b_session2:prev_points:prev_state_diffdifferent:serum_ami_high`) %>% sf

# diff rew_size 
(rew_size*post_sam$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`  + post_sam$`b_session2:prev_state_diffdifferent:ami_dummyAmi`+ 
    rew_size*post_sam$`b_session2:prev_points:ami_dummyAmi` + post_sam$`b_session2:ami_dummyAmi`) %>% sf()

(post_sam$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi` +post_sam$`b_session2:prev_points:ami_dummyAmi` ) %>% sf


# same rew_size 
(rew_size*post_sam$`b_session2:prev_points:ami_dummyAmi` + post_sam$`b_session2:ami_dummyAmi`) %>% sf()
(post_sam$`b_session2:prev_points:ami_dummyAmi` ) %>% sf()
(post_sam$`b_session2:prev_points:ami_dummyAmi`  + post_sam$`b_session2:prev_points:serum_ami_high`) %>% sf()


# rest ####
# fit_model_beh2 <- readRDS("../Brms_stay_beh.rds")
fit_model_beh3 <- readRDS("Brms_points_beh_prevpts_full.rds")
# fit_model_beh3 <- readRDS("Brms_points_beh_winloss_Ses2.rds")

 
# reward levels #########

post_sam <- posterior_samples(fit_model_beh, pars = c("^b_", "sd_", "sigma"))

# get the regressiors
beta_mat <- post_sam %>% 
  transmute(h_beta_prevpt = (b_prev_points+ 1/2*`b_session2:prev_points` + 
                             `b_prev_points:nal_dummyNal` + `b_prev_points:ami_dummyAmi`  +
                               1/2*`b_session2:prev_points:nal_dummyNal` +1/2*`b_session2:prev_points:ami_dummyAmi`),
         g_beta_pp_prevpt= (`b_prev_points:prev_state_diffdifferent` +1/2*`b_session2:prev_points:prev_state_diffdifferent`+
                              1/2* `b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal` +1/2*`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi` +
                               `b_prev_points:prev_state_diffdifferent:nal_dummyNal` +`b_prev_points:prev_state_diffdifferent:ami_dummyAmi`),
         e_delta_pp_prevpt_ami_same= (`b_session2:prev_points:ami_dummyAmi` ),
         d_delta_pp_prevpt_ami_different= (`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi` +
         `b_session2:prev_points:ami_dummyAmi` ),
         f_delta_pp_prevpt_ss_ami= `b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`,
         b_delta_pp_prevpt_nal_same = `b_session2:prev_points:nal_dummyNal`,
         a_delta_pp_prevpt_nal_different = ( `b_session2:prev_points:nal_dummyNal`+`b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal`),
         c_delta_pp_prevpt_ss_nal = `b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal`)
         
# print the regressors in odds scale :
beta_mat %>% exp() %>%  apply(2, sf) %>% t()
# in logodds scale 
beta_mat  %>%  apply(2, sf) %>% t()

## create the stats plot -----
g_stats <- beta_mat %>% select(h_beta_prevpt, g_beta_pp_prevpt, f_delta_pp_prevpt_ss_ami, c_delta_pp_prevpt_ss_nal) %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975),
            lls   = quantile(value, prob = .10),
            uls   = quantile(value, prob = .90)) %>%  # since the `key` variable is really two variables in one, here we split them up
  
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = name)) +
  geom_errorbar(aes(xmin = lls, xmax = uls), size = 1.5,color = "firebrick", width = 0)+
  geom_vline(xintercept = 0,  linetype = "dashed") +
  geom_pointrange(color = "firebrick") +
  labs( y = NULL) + # subtitle = "Effect sizes (95% and 50% quantiles)",
  theme_Publication(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        axis.title.x =  element_blank()) +
  scale_y_discrete(labels = rev(c("Prev points", "PrevPoints:PrevState", 
                                "Ami:Session:PrevPoints:PrevState",
                                 "Nal:Session:PrevPoints:PrevState")))


## plot beh model posterior predictions  -------------------------------------------------------

post <-  posterior_samples(fit_model_beh)

# define helper functions
post_fu <- function(x, funct_var) {
  prev_points = x[1]
  ami_dummy =  as.numeric(x[2] == 1)
  nal_dummy =  as.numeric(x[2] == 2)
  prev_state =  x[3]
  session =  x[4]
  fitted = post$b_Intercept +  post$b_prev_points * prev_points +  post$b_prev_state_diffdifferent*prev_state+  post$b_session2*session +  post$b_ami_dummyAmi*ami_dummy + post$b_nal_dummyNal*nal_dummy+
    
    post$`b_session2:prev_points`* prev_points*session +  post$`b_session2:prev_state_diffdifferent`*session*prev_state + post$`b_prev_points:prev_state_diffdifferent`*prev_points*prev_state+
    post$`b_session2:ami_dummyAmi`*session*ami_dummy + post$`b_prev_points:ami_dummyAmi`*prev_points*ami_dummy + post$`b_prev_state_diffdifferent:ami_dummyAmi`*ami_dummy*prev_state +
    post$`b_session2:nal_dummyNal`*session*nal_dummy+ post$`b_prev_points:nal_dummyNal`*prev_points*nal_dummy + post$`b_prev_state_diffdifferent:nal_dummyNal`*prev_state*nal_dummy +
    post$`b_session2:prev_points:prev_state_diffdifferent`*session*prev_state*prev_points + post$`b_session2:prev_points:ami_dummyAmi`*session*prev_points*ami_dummy +
    post$`b_session2:prev_state_diffdifferent:ami_dummyAmi`*prev_state*session*ami_dummy + post$`b_prev_points:prev_state_diffdifferent:ami_dummyAmi`*prev_state*prev_points*ami_dummy +
    post$`b_session2:prev_points:nal_dummyNal`*session*prev_points*nal_dummy + post$`b_session2:prev_state_diffdifferent:nal_dummyNal`*session*prev_state*nal_dummy + 
    post$`b_prev_points:prev_state_diffdifferent:nal_dummyNal`*prev_points*prev_state*nal_dummy + 
    post$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`*prev_points*prev_state*ami_dummy*session +
    post$`b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal`*prev_points*prev_state*nal_dummy*session
  
  # y_lo <- funct_var(fitted)
  y <- inv_logit(funct_var(fitted))
  return(y)
}

nd <- expand_grid(prev_points = c(-4:5), drug = c(1,2,3), prev_state = c(0,1), session = c(0,1) )
# nd <- nd   %>% mutate(median=NA, CI_l=NA, CI_u=NA)
nd_mat <- nd %>% as.matrix()
# nd_mat <- nd_mat
# nd_mean_CI = apply(nd_mat,1, post_fu)
nd_mean = apply(nd_mat,1, post_fu, funct_var = mean)
nd_lCI = apply(nd_mat,1, post_fu , funct_var = function(x) quantile(x, probs = 0.2))
nd_uCI = apply(nd_mat,1, post_fu, funct_var = function(x) quantile(x, probs = 0.8))
nd_sd = apply(nd_mat,1, post_fu, funct_var = sd)
nd_new <- nd %>% mutate(mean_value  = nd_mean,
                    low_CI  = nd_lCI,
                    up_CI  = nd_uCI,
                    sd_value  = nd_sd,
                    drug = factor(drug, levels= c(1,2,3), labels = c( "Ami","Nal", "Pla")),
                    session = factor(session+1)
                    # ami_dummy = factor(drug=="Ami", levels= c(FALSE,TRUE), labels = c("Pla", "Ami")),
                    # nal_dummy = factor(drug=="Ami", levels= c(0,1), labels = c("Pla", "Nal"))
                    )

data_beh <- data_beh%>% mutate(prev_points_5levels = floor(prev_points/2)*2 + 0.5)

data_same <- data_beh %>% filter(!is.na(stay), prev_state_diff == "same") %>%
  group_by(drug, prev_points,session) %>%
  summarize(N = n(), 
            mean_stay = mean(stay),
            se_stay = sd(stay)/sqrt(N))
data_diff<- data_beh %>% filter(!is.na(stay), prev_state_diff != "same") %>% 
  group_by(drug, prev_points, session) %>% 
  summarize(N = n(), 
            mean_stay = mean(stay),
            se_stay = sd(stay)/sqrt(N))


g_different <- nd_new %>% filter(prev_state == 1) %>% ggplot(aes(x= prev_points, y = mean_value, group = session)) + 
  geom_errorbar(data=data_diff, aes(x = prev_points, y = mean_stay,ymin = mean_stay - se_stay, ymax =mean_stay + se_stay),colour = "black", width = 0, position = position_dodge(0.2))+
  geom_point(data=data_diff, aes(x = prev_points, y = mean_stay, colour = session),position = position_dodge(0.2)) +
  geom_ribbon(aes(ymin = low_CI, ymax = up_CI, group = session), alpha = 0.2) +
  # geom_ribbon(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), alpha = 0.3) +
  geom_line(aes(colour = session)) + theme_Publication()  +   scale_color_manual(values=c('#999999','#E69F00'))+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks = element_blank(), strip.background=element_blank(), strip.text = element_blank())+ 
  xlab("Previous Points")+ ylab("P(stay) after \ndifferent first state")  + facet_wrap(~drug)  + coord_cartesian(ylim=c(0.20,1)) + 
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8)) +  geom_hline(yintercept =0.5, linetype = "dashed")


g_same <-  nd_new %>% filter(prev_state == 0) %>% ggplot(aes(x= prev_points, y = mean_value, group = session)) + 
  geom_errorbar(data=data_same, aes(x = prev_points, y = mean_stay,ymin = mean_stay - se_stay, ymax =mean_stay + se_stay),colour = "black", width = 0, position = position_dodge(0.2))+
  geom_point(data=data_same, aes(x = prev_points, y = mean_stay, colour = session),position = position_dodge(0.2)) +
  geom_ribbon(aes(ymin = low_CI, ymax = up_CI, group = session), alpha = 0.2) +
  # geom_ribbon(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), alpha = 0.3) +
  geom_line(aes(colour = session)) + theme_Publication()  +   scale_color_manual(values=c('#999999','#E69F00'))+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks = element_blank()) + xlab("Previous Points")+ 
  ylab("P(stay) after \nsame first state")  + facet_wrap(vars(drug))  + coord_cartesian(ylim=c(0.20,1)) +
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8)) + geom_hline(yintercept =0.5, linetype = "dashed")

# preview plot
# (g_same / g_different)
  


## plot beh model posterior predictions for session differences ----

post_fu_sess_difference <- function(x, funct_var) {
  prev_points = x[1]
  ami_dummy =  as.numeric(x[2] == 1)
  nal_dummy =  as.numeric(x[2] == 2)
  prev_state =  x[3]
  session =  0
  fitted_1 = post$b_Intercept +  post$b_prev_points * prev_points +  post$b_prev_state_diffdifferent*prev_state+  post$b_session2*session +  post$b_ami_dummyAmi*ami_dummy + post$b_nal_dummyNal*nal_dummy+
    
    post$`b_session2:prev_points`* prev_points*session +  post$`b_session2:prev_state_diffdifferent`*session*prev_state + post$`b_prev_points:prev_state_diffdifferent`*prev_points*prev_state+
    post$`b_session2:ami_dummyAmi`*session*ami_dummy + post$`b_prev_points:ami_dummyAmi`*prev_points*ami_dummy + post$`b_prev_state_diffdifferent:ami_dummyAmi`*ami_dummy*prev_state +
    post$`b_session2:nal_dummyNal`*session*nal_dummy+ post$`b_prev_points:nal_dummyNal`*prev_points*nal_dummy + post$`b_prev_state_diffdifferent:nal_dummyNal`*prev_state*nal_dummy +
    post$`b_session2:prev_points:prev_state_diffdifferent`*session*prev_state*prev_points + post$`b_session2:prev_points:ami_dummyAmi`*session*prev_points*ami_dummy +
    post$`b_session2:prev_state_diffdifferent:ami_dummyAmi`*prev_state*session*ami_dummy + post$`b_prev_points:prev_state_diffdifferent:ami_dummyAmi`*prev_state*prev_points*ami_dummy +
    post$`b_session2:prev_points:nal_dummyNal`*session*prev_points*nal_dummy + post$`b_session2:prev_state_diffdifferent:nal_dummyNal`*session*prev_state*nal_dummy + 
    post$`b_prev_points:prev_state_diffdifferent:nal_dummyNal`*prev_points*prev_state*nal_dummy + 
    post$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`*prev_points*prev_state*ami_dummy*session +
    post$`b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal`*prev_points*prev_state*nal_dummy*session
  
  session =  1
  fitted_2 = post$b_Intercept +  post$b_prev_points * prev_points +  post$b_prev_state_diffdifferent*prev_state+  post$b_session2*session +  post$b_ami_dummyAmi*ami_dummy + post$b_nal_dummyNal*nal_dummy+
    
    post$`b_session2:prev_points`* prev_points*session +  post$`b_session2:prev_state_diffdifferent`*session*prev_state + post$`b_prev_points:prev_state_diffdifferent`*prev_points*prev_state+
    post$`b_session2:ami_dummyAmi`*session*ami_dummy + post$`b_prev_points:ami_dummyAmi`*prev_points*ami_dummy + post$`b_prev_state_diffdifferent:ami_dummyAmi`*ami_dummy*prev_state +
    post$`b_session2:nal_dummyNal`*session*nal_dummy+ post$`b_prev_points:nal_dummyNal`*prev_points*nal_dummy + post$`b_prev_state_diffdifferent:nal_dummyNal`*prev_state*nal_dummy +
    post$`b_session2:prev_points:prev_state_diffdifferent`*session*prev_state*prev_points + post$`b_session2:prev_points:ami_dummyAmi`*session*prev_points*ami_dummy +
    post$`b_session2:prev_state_diffdifferent:ami_dummyAmi`*prev_state*session*ami_dummy + post$`b_prev_points:prev_state_diffdifferent:ami_dummyAmi`*prev_state*prev_points*ami_dummy +
    post$`b_session2:prev_points:nal_dummyNal`*session*prev_points*nal_dummy + post$`b_session2:prev_state_diffdifferent:nal_dummyNal`*session*prev_state*nal_dummy + 
    post$`b_prev_points:prev_state_diffdifferent:nal_dummyNal`*prev_points*prev_state*nal_dummy + 
    post$`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`*prev_points*prev_state*ami_dummy*session +
    post$`b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal`*prev_points*prev_state*nal_dummy*session
  
  # y_lo <- funct_var(fitted)
  y <- inv_logit(fitted_2) - inv_logit(fitted_1)
  y <- funct_var(y)
  return(y)
}

nd_ses <- expand_grid(prev_points = c(-4:5), drug = c(1,2,3), prev_state = c(0,1))
nd_mat <- nd_ses %>% as.matrix()

nd_mean = apply(nd_mat,1, post_fu_sess_difference, funct_var = mean)
nd_lCI = apply(nd_mat,1, post_fu_sess_difference , funct_var = function(x) quantile(x, probs = 0.2))
nd_uCI = apply(nd_mat,1, post_fu_sess_difference, funct_var = function(x) quantile(x, probs = 0.8))
# nd_sd = apply(nd_ses,1, post_fu, funct_var = sd)
nd_new <- nd_ses %>% mutate(mean_value  = nd_mean,
                        low_CI  = nd_lCI,
                        up_CI  = nd_uCI,
                        # sd_value  = nd_sd,
                        drug = factor(drug, levels= c(1,2,3), labels = c( "Ami","Nal", "Pla")))



## plot difference in staying across 5 reward levels and overlay the model predictions over it -------------------------
### reward level (-2,-1,0,1,2)
data_beh$reward_level <- floor((data_beh$prev_points + 4)/2) -2 
g_stay_session_diff <- data_beh %>%
  filter(!is.na(prev_points))%>%
  group_by(ID, session, reward_level, prev_state_diff, stay) %>%
  summarise(N=n(), ami_dummy = ami_dummy[1],
            nal_dummy = nal_dummy[1],
            drug = drug[1],
            dat= dat[1],
            darpp = darpp[1])  %>%
  mutate(freq = N/sum(N)) %>%
  filter(stay==1) %>%
  mutate(freq_diff = -10)
# View(g_stay_session_diff)
subjects = unique(g_stay_session_diff$ID)
reward_levels = c(-2,-1,0,1,2)
for (n in subjects) {
  # print(n)
  for (r in reward_levels) {
    if (sum(g_stay_session_diff$session[g_stay_session_diff$ID ==n &
                                        g_stay_session_diff$reward_level ==r] == 1) ==2 &
        sum(g_stay_session_diff$session[g_stay_session_diff$ID ==n &
                                        g_stay_session_diff$reward_level ==r] == 2) ==2) {
      
      g_stay_session_diff$freq_diff[g_stay_session_diff$session==2 & g_stay_session_diff$ID ==n &
                                      g_stay_session_diff$reward_level ==r] <- g_stay_session_diff$freq[g_stay_session_diff$session==2 & g_stay_session_diff$ID ==n &
                                                                                                         g_stay_session_diff$reward_level ==r] -
        g_stay_session_diff$freq[g_stay_session_diff$session==1 &
                                   g_stay_session_diff$ID ==n &
                                   g_stay_session_diff$reward_level ==r]
      
    } else
      cat("sub ", n, "and reward ", r, "\n")
    
    
  }
}

g_stay_session_diff$reward_level <- as.factor(g_stay_session_diff$reward_level )
g_beh_diff_stay <- g_stay_session_diff %>% 
  filter(session ==2, freq_diff !=-10, prev_state_diff=="different")  %>%
  group_by(drug, reward_level) %>% summarize(N=n(),
                                            mean_freq_diff = mean(freq_diff, na.rm = TRUE),
                                            se_freq_diff = sd(freq_diff, na.rm = TRUE)/sqrt(N)) 

nd_new_diff <- nd_new %>% filter(prev_state == 1)
# nd_new_1 %>% View()
g_different_diff <-
  g_beh_diff_stay %>% mutate(reward_level = as.numeric(reward_level)*2 - 5.5) %>%
  ggplot(aes(x=reward_level, y = mean_freq_diff, fill = drug)) +
   geom_ribbon(data =nd_new_diff, aes(x= prev_points, y = mean_value, ymin = low_CI, ymax = up_CI, group = drug), alpha = 0.2) +
   geom_line(data = nd_new_diff, aes(x= prev_points, y = mean_value, colour = drug), colour = "black") +
  geom_point(aes(colour = drug),stat = "identity", position = position_dodge(width = 0.1),  size = 3)+
  geom_hline(yintercept =0, linetype = "dashed")+
  geom_errorbar(aes(ymin= mean_freq_diff - se_freq_diff, ymax = mean_freq_diff +se_freq_diff), width = 0, position = position_dodge(0.9))+
  # scale_x_discrete(breaks = NULL) + #labels = c("-2"="-4","-1"= "","0"= "0", "1" = "", "2"="5"))+
  # scale_y_continuous(breaks = c(0,0.1,-0.1), limits= c(-0.15,0.15)) +
  facet_wrap(vars(drug)) +  ylab("\u0394 P(stay) after\ndifferent first state") +# labs(subtitle = "Different first state") +
  theme_Publication() + theme(legend.position = "none", strip.background=element_blank(), strip.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
  xlab("Previous Points") +    coord_cartesian(ylim = c(-0.1,0.15)) + scale_y_continuous(breaks = c(-0.1, 0.0, 0.1))



# (aes(x = drug, y= mean_par_t0, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3
g_beh_same_stay <- g_stay_session_diff %>% 
  filter(session ==2, freq_diff !=-10, prev_state_diff=="same")  %>%
  group_by(drug, reward_level) %>% summarize(N=n(),
                                            mean_freq_diff = mean(freq_diff, na.rm = TRUE),
                                            se_freq_diff = sd(freq_diff, na.rm = TRUE)/sqrt(N))
nd_new_same<- nd_new %>% filter(prev_state == 0)
g_same_diff <-
  g_beh_same_stay %>% mutate(reward_level = as.numeric(reward_level)*2 - 5.5) %>%
  ggplot(aes(x=reward_level, y = mean_freq_diff, fill = drug)) +
  geom_ribbon(data =nd_new_same, aes(x= prev_points, y = mean_value, ymin = low_CI, ymax = up_CI, group = drug), alpha = 0.2) +
  geom_line(data = nd_new_same, aes(x= prev_points, y = mean_value, colour = drug), colour = "black") +
  geom_point(aes(colour = drug),stat = "identity", position = position_dodge(width = 0.1),  size = 3)+
  geom_hline(yintercept =0, linetype = "dashed")+
  geom_errorbar(aes(ymin= mean_freq_diff - se_freq_diff, ymax = mean_freq_diff +se_freq_diff), width = 0, position = position_dodge(0.9))+
  # scale_x_discrete(breaks = NULL) + #labels = c("-2"="-4","-1"= "","0"= "0", "1" = "", "2"="5"))+
  # scale_y_continuous(breaks = c(0,0.1,-0.1), limits= c(-0.15,0.15)) +
  facet_wrap(vars(drug)) + ylab("\u0394 P(stay) after\nsame first state") + #labs(subtitle = "Different first state") +
   theme_Publication() + theme(legend.position = "none",axis.text.x = element_blank(), axis.ticks = element_blank())+
  xlab("Previous Points") + coord_cartesian(ylim = c(-0.1,0.15)) + scale_y_continuous(breaks = c(-0.1, 0.0, 0.1))

## plot figure 3: behavior ----

plot_grid(plot_grid(g_same/g_different, g_same_diff/g_different_diff, nrow = 1 , labels = c("a", "b")),
          g_stats + theme(axis.text = element_text(size = 10), panel.grid.major.y = element_line( size=.1, color="black", linetype = "dashed")),
          nrow = 2,
          rel_heights = c(1,0.3), labels = c("", "c"))

## stay with amisulpride serum levels -----
fit_model_beh_serum <- readRDS("Brms_stay_beh_serum.rds")
post_sam <- posterior_samples(fit_model_beh_serum, pars = c("^b_", "sd_", "sigma"))
# get the regressiors

beta_mat <- post_sam %>% 
  transmute(i_delta_pp_prevpt_ami_same_low_serum= (`b_session2:prev_points:ami_dummyAmi` ),
            h_delta_pp_prevpt_ami_different_low_serum = (`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi` +
                                                `b_session2:prev_points:ami_dummyAmi` ),
            g_delta_pp_prevpt_ss_ami_low_serum= `b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`,
            f_delta_pp_prevpt_ami_same_high_serum= (`b_session2:prev_points:ami_dummyAmi` +`b_session2:prev_points:serum_ami_high`),
            e_delta_pp_prevpt_ami_different_high_serum = (`b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi` +
                                                           `b_session2:prev_points:ami_dummyAmi`+`b_session2:prev_points:serum_ami_high`+
                                                            `b_session2:prev_points:prev_state_diffdifferent:serum_ami_high`),
            d_delta_pp_prevpt_ss_ami_high_serum= `b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi`+
              `b_session2:prev_points:prev_state_diffdifferent:serum_ami_high`,
            c_delta_pp_prevpt_same_ami_serum_diff= `b_session2:prev_points:serum_ami_high`,
            b_delta_pp_prevpt_different_ami_serum_diff=  `b_session2:prev_points:prev_state_diffdifferent:serum_ami_high`,
            a_delta_pp_prevpt_ss_ami_serum_diff=  `b_session2:prev_points:prev_state_diffdifferent:serum_ami_high`+`b_session2:prev_points:serum_ami_high`
            )

# print the regressors in odds scale :
beta_mat %>% exp() %>%  apply(2, sf) %>% t()
# in logodds scale 
beta_mat  %>%  apply(2, sf) %>% t()

for (col in  1 : dim(beta_mat)[2]) {
  print(paste(colnames(beta_mat)[col], beta_mat[,col ] %>% sf(1)))
}
#############################################################################################################
# Computational modelling  ########################################################################
## stan models were estimated using the analysis_main.r scripts and custom written stan models


## run all stan models

if (FALSE) { # this will take some time - already estimated models are available upon request (nace.mikus@univie.ac.at)
  # M1 model
  run_model_fit(modelfile = "Stan Scripts/M_new.stan", "Stan Model Results/M_new.rds") 
  # kool 1lr model 
  run_model_fit(modelfile = "Stan Scripts/M_kool_1lr.stan", "Stan Model Results/M_kool_1lr.rds") 
  # kool 2lr model 
  run_model_fit(modelfile = "Stan Scripts/M_kool_2lr.stan", "Stan Model Results/M_kool_2lr.rds") 
  
  # M1 model with serum
  run_model_fit(modelfile = "Stan Scripts/M_new_serum.stan", "Stan Model Results/M_new_serum.rds") 
  # M1 model with serum and stickiness parameters 
  run_model_fit(modelfile = "Stan Scripts/M_new_sticky_serum.stan", "Stan Model Results/M_new_sticky_serum.rds")
}

## posterior distributions of parameters from M1 model ##################
fit_model <- readRDS("Stan Model Results/M_new.rds")
pars_all <- grep("^beta", names(fit_model), value = T)

pars_all <- pars_all[1:6]

# glimpse results

stan_plot(fit_model, pars_all)

# extract the numbers

Par_extract = rstan::extract(fit_model, pars = c('mu_p', 'sigma', 'sess_w', 'w_mean', 'beta_nal', 
                                                 'beta_ami', 'w', 'L_Omega','beta_ami_noise', 'beta_nal_noise',
                                                 'beta_ami_g', 'beta_nal_g'))

# in parameter estimation space:

w_ami <- Par_extract$beta_ami
w_ami %>% sf(1)

w_nal <- Par_extract$beta_nal 
w_nal %>% sf(1)
g_ami <- Par_extract$beta_ami_g
g_nal <- Par_extract$beta_nal_g
b_ami<- Par_extract$beta_ami_noise
b_ami %>% sf(1)
b_nal <- Par_extract$beta_nal_noise

# effect sizes  
sd_w_ses =  Par_extract$sigma[5]
sd_w =  Par_extract$sigma[2]

sd_b_ses =  Par_extract$sigma[4]
sd_b =  Par_extract$sigma[1]

sd_g_ses =  Par_extract$sigma[6]
sd_g =  Par_extract$sigma[3]

d_ami_w <- (Par_extract$beta_ami/sqrt(sd_w_ses^2 + sd_w^2))  
d_ami_w %>% sf(1)

d_nal_w <- (Par_extract$beta_nal/sqrt(sd_w_ses^2 + sd_w^2))

d_diff_ami_nal_w <- (Par_extract$beta_ami - Par_extract$beta_nal)/sqrt(sd_w_ses^2 + sd_w^2)
(Par_extract$beta_ami - Par_extract$beta_nal) %>% sf(1)
d_ami_noise <- Par_extract$beta_ami_noise/sqrt(sd_b_ses^2 + sd_b^2)
d_ami_noise %>% sf(1)
d_nal_noise <- Par_extract$beta_nal_noise/sqrt(sd_b_ses^2 + sd_b^2)

d_ami_g <- Par_extract$beta_ami_g/sqrt(sd_g_ses^2 + sd_g^2)

d_nal_g <- Par_extract$beta_nal_g/sqrt(sd_g_ses^2 + sd_g^2)



temp_mat <- bind_cols(A =  d_nal_g,
                      B =  d_ami_g,
                      C =  d_nal_noise,
                      D =  d_ami_noise,
                      E =  d_diff_ami_nal_w,
                      F =  d_nal_w,
                      G =  d_ami_w)


g_stats <- temp_mat %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975),
            lls   = quantile(value, prob = .10),
            uls   = quantile(value, prob = .90)) %>%  # since the `key` variable is really two variables in one, here we split them up
  
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = name)) +
  geom_errorbar(aes(xmin = lls, xmax = uls), size = 1.5,color = "firebrick", width = 0)+
  geom_vline(xintercept = 0,  linetype = "dashed") +
  geom_pointrange(color = "firebrick") +
  labs( y = NULL) + # subtitle = "Effect sizes (95% and 50% quantiles)",
  theme_Publication(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +
  scale_y_discrete(labels = rev(c("Ami \U1D714","Nal \U1D714","Ami - Nal \U1D714","Ami \U1D702","Nal \U1D702","Ami \U1D6FE","Nal \U1D6FE")))
g_stats + xlab("Effect sizes (95% and 80% CI)")

# d_diff_ami_nal_w %>% sf(1)

# phi_approx <- function(x) inv_logit(0.07056*x^3 + 1.5976*x)
## results M1 (figure 4)    ----------------------------------------------------

par ="w"

# parname = "\U1D714"
plot_par_results <- function(par, data= data_group, title_text = list(NA,NA,NA)) {
  g= list()
  parname = case_when(par == "w"~ "\U1D714",
                      par == "beta1" ~ "\U1D702",
                      par == "g" ~  "\U1D6FE",
                      TRUE~  par)
  
  par_rstan <- paste(par,"rstan",sep= "_")
  par_t0_rstan <- paste(par,"t0_rstan",sep= "_")
  # par_rstan <- paste(par,"raw",sep= "_")
  # par_t0_rstan <- paste(par,"t0_raw",sep= "_")
  par_sess <- paste("sess",par,"rstan", sep="_")
  
  # data %>% dplyr::select(drug == 1) %>%
  # data.frame(w= data$w_rstan,w_raw = data$w_raw) %>% View()
  par_rstan <- sym(par_rstan)
  par_t0_rstan <- sym(par_t0_rstan)
  par_sess <- sym(par_sess) 
  
  
  
  data_barplot <- data %>% 
    group_by(drug) %>%
    summarise(N = n(),
              mean_par = mean(!!par_rstan),
              mean_par_t0 = mean(!!par_t0_rstan),
              se_par = sd(!!par_rstan)/sqrt(N), 
              se_par_t0 = sd(!!par_t0_rstan)/sqrt(N),
              max_par = max(c(!!par_rstan, !!par_t0_rstan)),
              min_par = min(c(!!par_rstan, !!par_t0_rstan)))
  # View(data_barplot)
  min_par_no <-min(min(data_barplot$min_par) - 0.1*abs(min(data_barplot$min_par)) ,0)
  max_par_no <- max(max(data_barplot$max_par) + 0.1*abs(max(data_barplot$max_par)) ,1)
  
jw_par = 1
trans_par = 0.2
g[[1]] <- ggplot(data_barplot, aes(x = drug, y= mean_par_t0, fill = drug))+
  # geom_bar(position = position_dodge(), stat = "identity") + 
  geom_point(data = data, aes(x=drug, y = !!par_rstan, fill = drug), alpha =trans_par, position = position_jitterdodge(dodge.width= 0.9, jitter.width =jw_par), shape = 21, colour = "black", size = 2, stroke =1) +  
  
  geom_errorbar(aes(ymin= mean_par_t0 - se_par_t0, ymax = mean_par_t0+se_par_t0), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(x = drug, y= mean_par_t0, colour = drug),  position = position_dodge(width = 0.1), stat = "identity",  size = 3, shape = 21, colour = "black") +
  
  ylab(paste(parname, 'in session 1') ) + #ylim(min_par_no, max_par_no) + 
  theme_Publication() +labs(subtitle = title_text[[1]])+
  theme(legend.position = "none", plot.subtitle = element_text(size = rel(0.8), hjust = 0),
        axis.line = element_line(colour = "black"), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())  + 
  scale_y_continuous(breaks = c(0, 0.5,1), limits= c(0,1))
# +
# scale_x_discrete(breaks=c("-4","0","5"))

# g2
# plot.subtitle = element_text(size = rel(rel_height), hjust = 0), 
g[[2]] <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug)) +
  # geom_bar(position = position_dodge(), stat = "identity") +
  # geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
  geom_point(data = data, aes(x=drug, y = !!par_rstan, fill = drug), alpha = trans_par, position = position_jitterdodge(dodge.width= 0.9, jitter.width = jw_par), shape = 21, colour = "black", size = 2, stroke =1) +  
  
  geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0.2, position = position_dodge(0.9)) +
  # geom_point(data = data_change_id, aes(x = Backtransfer_f, y = Change, fill= Treatment, colour= Treatment),  alpha = 0.3, position = position_jitterdodge(dodge.width= 0.9, jitter.width = 0.3), shape = 21, colour = "black", size = 2, stroke =1)+
  geom_point(aes(x = drug, y= mean_par, colour = drug),  position = position_dodge(width = 0.1), stat = "identity",  size = 3, shape = 21, colour = "black") +
  
  theme(legend.position = "none") + 
  #ylim(min_par_no, max_par_no)  +

  ylab(paste(parname, ' in session 2')) + 
  theme_Publication()+labs(subtitle = title_text[[2]])+
  theme(legend.position = "none", plot.subtitle = element_text(size = rel(0.8), hjust = 0),
        axis.line = element_line(colour = "black"), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())  + 
  scale_y_continuous(breaks = c(0, 0.5,1), limits= c(0,1))


# g1




g[[3]]  <- ggplot(data = data, aes(x = drug, y = !!par_sess))+  # group = ID, linetype = Genotype)) 
  # geom_violin(aes(fill = drug), alpha = 0.3, width = 0.5, colour = "NA" )+
  geom_boxplot(aes(fill = drug), width=0.5, color="black", alpha=0.9, outlier.shape=NA)+
  
  geom_point(data = data, aes(x=drug, y = !!par_sess, fill = drug), alpha = trans_par, position = position_jitterdodge(dodge.width= 0.9, jitter.width = jw_par), shape = 21, colour = "black", size = 2, stroke =1) +  
  
  # geom_jitter(size = 0.5, width = 0.2) + 
  theme_Publication()+labs(subtitle = title_text[[3]])+
  theme(legend.position = "none", plot.subtitle = element_text(size = rel(0.8), hjust = 0),
        axis.line = element_line(colour = "black"), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) + ylab(paste0("\u0394", parname, "'")) 
# g

return(g)
}
g_b <- plot_par_results("beta1", title_text = list(NA, NA, "Inverse temperature") )
g_g <- plot_par_results("g", title_text =list("", "", "Devaluation parameter"))
# g_b_s <- plot_par_results("beta1", data = data_group%>% filter(!is.na(serum_f)))
g_w <- plot_par_results("w", title_text = list("Model-based/model-free weight", "",""))
# g_w_s <- plot_par_results("w", data = data_group%>% filter(!is.na(serum_f)))
# g_b_s[[3]]+facet_wrap(~serum_f)

# g_b_s[[2]]+facet_wrap(~serum_f)

g_all <-  plot_grid(g_w[[1]],
                    g_w[[2]],
                    g_w[[3]],
                    label_x = 0.5,
                    rel_widths = c(1,1,1),
                    ncol = 3)


g_fig_3 <- plot_grid(g_w[[1]],
                      g_w[[2]],
                      g_w[[3]],
                     g_b[[3]],
                     g_g[[3]],
  g_stats + xlab("Effect sizes"), ncol = 3, labels = c("a","", "",  "b", "c", "d")
)
g_fig_3
ggsave("g_all_admin_model.png", plot = g_all, device = NULL, path = NULL,
       scale = 1, width = 10, height = 5, dpi = 300, limitsize = TRUE)




## correct predictions + plot -------------------------------------------------------
path_to_stan = ""


get_correct_predictions <- function(fit_model) {

# add_criterion(M_g_final, waic)
# fit_model <- M_g_admin #M_g_final
y_pred_mats <- rstan::extract(fit_model, pars = c("y_pred", "y_pred_t0"))

# y_pred_mats
# id <- 45 #110
df = tibble(
  perc_correct  = rep(0, length=length(subjList)),
  perc_correct_mat  = rep(0, length=length(subjList)) ,
  perc_correct_t0  = rep(0, length=length(subjList)) ,
  perc_correct_mat_t0  = rep(0, length=length(subjList)) 
 )

for (l in 1:length(subjList)) {
  data_id <- data_beh %>%
  filter(s == subjList[l], session == 2) %>% mutate(choice01 = choice12-1)
 
  y_pred_id <- y_pred_mats$y_pred %>% colMeans()
  y_pred_id <- y_pred_id[l ,data_id$trials]
  corr_vect <- data_id$choice01 == as.numeric(y_pred_id >0.5)
  # # y_pred_id_same <- y_pred_id[data_id$prev_state_diff=="same"]
  # y_pred_id_diff  <- y_pred_id[data_id$prev_state_diff=="different"]
  y_pred_id <- y_pred_mats$y_pred
  y_pred_id <- y_pred_id[,l ,data_id$trials] 
  
  corr_mat <- apply(y_pred_id, 1, function(x) x == data_id$choice01) %>% t()
  # rbind(rbind(corr_mat, data_id$choice01), y_pred_id) %>% View()
  data_id_t0<- data_beh %>%
    filter(s == subjList[l], session == 1) %>% mutate(choice01 = choice12-1)
  y_pred_id_t0 <- y_pred_mats$y_pred_t0 %>% colMeans()
  y_pred_id_t0 <- y_pred_id_t0[l ,data_id_t0$trials]
  corr_vect_t0 <- data_id_t0$choice01 == as.numeric(y_pred_id_t0 >0.5)
  # # y_pred_id_same <- y_pred_id[data_id$prev_state_diff=="same"]
  # y_pred_id_diff  <- y_pred_id[data_id$prev_state_diff=="different"]
  y_pred_id_t0 <- y_pred_mats$y_pred_t0
  y_pred_id_t0 <- y_pred_id_t0[,l ,data_id_t0$trials] 
  
  corr_mat_t0 <- apply(y_pred_id_t0, 1, function(x) x == data_id_t0$choice01) %>% t()
 
 
 
  df$perc_correct[l] <- mean(corr_vect)
  df$perc_correct_mat[l] <- sum(corr_mat)/length(corr_mat) 
  df$perc_correct_t0[l] <- sum(corr_vect_t0)/length(data_id_t0$choice01)
  df$perc_correct_mat_t0[l] <- sum(corr_mat_t0)/length(corr_mat_t0) 
  # perc_correct_same[l] <- sum( corr_vect[data_id$prev_state_diff=="same"])/length(corr_vect[data_id$prev_state_diff=="same"])
  # perc_correct_diff[l] <-  sum(corr_vect[data_id$prev_state_diff=="different"])/length(corr_vect[data_id$prev_state_diff=="different"])

  }

return(df) 
}
df_m1 = get_correct_predictions(fit_model)
data_group$perc_correct = df_m1$perc_correct
# 
## plot the perc correctgraph 

g <- ggplot(data = data_group, aes(x = drug, y =perc_correct))+  # group = ID, linetype = Genotype)) 
  # geom_violin(aes(fill = drug))+
  geom_boxplot(aes(fill = drug), width=0.5, color="grey", alpha=1)+
  geom_jitter(size = 0.1) + 
  theme_minimal(base_size = 25, )   + 
  theme(legend.position = "none", plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
        axis.line = element_line(colour = "black")) + ylab("% correct prediction") + scale_x_discrete(name = "")
g


if(!file.exists("brms_perc_correct.rds")) {
  brms_perc_correct<- brm(log(perc_correct/(1-perc_correct)) ~ ami_dummy + nal_dummy,
                             data = data_group,
                             family = gaussian,
                             prior = c(set_prior("normal(0,3)", class = "b")),
                             warmup = 500, iter = 2000, chains = 4)
  saveRDS(brms_perc_correct, file ="brms_perc_correct.rds")
} else {brms_perc_correct <- readRDS(file ="brms_perc_correct.rds") }
  

brms_perc_correct %>% fixef() %>% round(2) %>%  write.csv(file = "model_perc_correct.csv")

## generate posterior predictions of the stan models  -------------------------------------------------

# individual s = 26, with all trials, for predictions.
data_id_wow <- data_beh[data_beh$s == 26,]

no_of_samples <- 1 # no of parameter sets to samples per individual

if (FALSE) source("Util_scripts/Model_stimulations.r") ## takes some time, need this to produce data_pred

for (l in 1:length(subjList)) { #
  print(l)
  data_id <- y_pred[1:no_of_samples,l,]
  data_id_t0 <- y_pred_t0[1:no_of_samples,l,]
  no_of_trials = length(y_pred_t0[1,l,])
  reward_id <- y_reward[1:no_of_samples,l,]
  reward_id_t0 <- y_reward_t0[1:no_of_samples,l,]
  # 
  # data_pred_id <- tibble(
  #   PredChoice01 = as.vector(t(data_id_t0)),
  #   PredPrevChoice01 = lag(as.vector(t(data_id_t0))),
  #   PredStay = as.vector(t(data_id_t0)) == lag(as.vector(t(data_id_t0))),
  #   sim = as.vector(t(matrix(rep(1: dim(data_id_t0)[1], no_of_trials),dim(data_id_t0)[1]))),
  #   count_sim = (l-1)*no_of_samples + as.vector(t(matrix(rep(1: dim(data_id_t0)[1], no_of_trials),dim(data_id_t0)[1]))),
  #   trial =rep(1:no_of_trials,dim(data_id_t0)[1]),
  #   s= subjList[l],
  #   drug = drug_id_t0,
  #   session = 1)
  # View(data_pred_id)
  drug_id_t0 <- unique(data_beh$drug[data_beh$s == subjList[l] & data_beh$session ==1])
  drug_id <- unique(data_beh$drug[data_beh$s == subjList[l]  & data_beh$session ==2])
 
  data_pred_id <- rbind(tibble(
    PredChoice01 = as.vector(t(data_id_t0)),
    PredPrevChoice01 = lag(as.vector(t(data_id_t0))),
    PredStay = as.vector(t(data_id_t0)) == lag(as.vector(t(data_id_t0))),
    sim = as.vector(t(matrix(rep(1: no_of_samples, no_of_trials),no_of_samples))),
    count_sim = (l-1)*no_of_samples + as.vector(t(matrix(rep(1: no_of_samples, no_of_trials),no_of_samples))),
    # prev_points = rep(data_id_wow$prev_points[data_id_wow$session == 1],no_of_samples), 
    points =  as.vector(t(reward_id_t0)),
    prev_points = lag(as.vector(t(reward_id_t0))),
    prev_state_diff = rep(data_id_wow$prev_state_diff[data_id_wow$session == 1],no_of_samples), 
    trial =rep(1:no_of_trials,no_of_samples), 
    s= subjList[l],
    drug = drug_id_t0,
    session = 1),
    tibble(
      PredChoice01 = as.vector(t(data_id)),
      PredPrevChoice01 = lag(as.vector(t(data_id))),
      PredStay = as.vector(t(data_id)) == lag(as.vector(t(data_id))),
      sim = as.vector(t(matrix(rep(1: no_of_samples, no_of_trials),nrow=no_of_samples))),
      # prev_points = rep(data_id_wow$prev_points[data_id_wow$session == 2],no_of_samples), 
      points =  as.vector(t(reward_id)),
      prev_points = lag(as.vector(t(reward_id))),
      prev_state_diff = rep(data_id_wow$prev_state_diff[data_id_wow$session == 2],no_of_samples), 
      trial =rep(1:no_of_trials,no_of_samples), 
      s= subjList[l],
      count_sim = (l-1)*no_of_samples + as.vector(t(matrix(rep(1: no_of_samples, no_of_trials),no_of_samples))),
      drug = drug_id,
      session = 2) )
  
  data_pred <- rbind(data_pred,data_pred_id)
  
}

data_pred <- within(data_pred, {
  ID_sim <- factor(count_sim)
  nal_dummy<-factor(drug == "Nal", levels = c(FALSE,TRUE), labels = c("Pla","Nal" ))
  ami_dummy<-factor(drug == "Ami", levels =  c(FALSE,TRUE), labels = c("Pla","Ami" ))
})

## plot model predictions straight lines -----------------------------------

data_pred <- data_pred %>% mutate(ID = count_sim, stay = PredStay)
data_pred$session <- as.factor(data_pred$session)

g1 <- data_pred %>%
  filter(prev_state_diff == "different") %>%
  group_by(ID, session, prev_points, stay) %>%
  dplyr::summarize(N=n(),
                   ami_dummy = ami_dummy[1],
                   nal_dummy = nal_dummy[1],
                   drug = drug[1])%>%
  mutate(freq=N/sum(N)) %>%
  filter(stay == 1) %>% ggplot( aes(x = prev_points, y = freq,group = session, color = session)) +
  geom_smooth(method=lm)+
  # ylim(c(0,1))+#, se = TRUE)  +
  scale_x_continuous(breaks=NULL)+
  scale_y_continuous(breaks=c(0.3,0.5,0.7), limits = c(0,1))+
  facet_wrap(vars(drug))+
  #"#F8766D" "#00BA38" "#619CFF
  scale_color_manual(values=c('#999999','#E69F00'))+
  labs(x = "", y= "P(stay)")+
  theme_minimal(base_size=25) + 
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 18)) + coord_cartesian(ylim=c(0.25, 1))


g2 <- data_pred %>%
  filter(prev_state_diff == "same") %>%
  group_by(ID, session, prev_points, stay) %>%
  summarize(N=n(),
            ami_dummy = ami_dummy[1],
            nal_dummy = nal_dummy[1],
            drug = drug[1]) %>%
  mutate(freq=N/sum(N)) %>%
  filter(stay == 1)%>%
  ggplot(aes(x = prev_points, y = freq, color = session)) +
  geom_smooth(method=lm)+
  theme(axis.text.x = element_text(face="bold", size = 15))  +
  scale_x_continuous(breaks=NULL)+
  scale_y_continuous(breaks=c(0.3,0.5,0.7), limits = c(0,1))+
  facet_wrap(vars(drug)) +
  #"#F8766D" "#00BA38" "#619CFF
  scale_color_manual(values=c('#999999','#E69F00'))+
  labs(x = "Previous Points", y= "P(stay)")+
  theme_minimal(base_size=25) + 
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=18)) + coord_cartesian(ylim=c(0.25,1))
g2

g_stay_lines  <- plot_grid(g1,
                           g2,
                           label_x = 0.5,
                           nrow = 2)
g_stay_lines

# data_pred %>% glimpse()
# data_pred$ID %>% unique()
## plot model predictions over data... ----
data_pred <- data_pred %>% mutate(ID = count_sim, stay = PredStay)
data_pred$session <- as.factor(data_pred$session)



data_same_model <- data_pred %>% filter(!is.na(stay), prev_state_diff == "same") %>%
  group_by(drug, prev_points,session) %>%
  summarize(N = n(), 
            mean_stay = mean(stay),
            se_stay = sd(stay)/sqrt(N))

data_diff_model<- data_pred %>% filter(!is.na(stay), prev_state_diff != "same") %>% 
  group_by(drug, prev_points, session) %>% 
  summarize(N = n(), 
            mean_stay = mean(stay),
            se_stay = sd(stay)/sqrt(N))

data_same <- data_beh %>% filter(!is.na(stay), prev_state_diff == "same") %>%
  group_by(drug, prev_points,session) %>%
  summarize(N = n(), 
            mean_stay = mean(stay),
            se_stay = sd(stay)/sqrt(N))
data_diff<- data_beh %>% filter(!is.na(stay), prev_state_diff != "same") %>% 
  group_by(drug, prev_points, session) %>% 
  summarize(N = n(), 
            mean_stay = mean(stay),
            se_stay = sd(stay)/sqrt(N))
# data_diff_model$drug %>% glimpse()
# data_beh$drug %>% glimpse()
g_different_model <- ggplot(data=data_diff, aes(x= prev_points, y = mean_stay, group = session)) + 
  geom_errorbar(aes(x = prev_points, y = mean_stay,ymin = mean_stay - se_stay, ymax =mean_stay + se_stay),colour = "black", width = 0, position = position_dodge(0.2))+
  geom_point(aes(x = prev_points, y = mean_stay, colour = session),position = position_dodge(0.2)) +
  geom_ribbon(data=data_diff_model, aes(ymin = mean_stay - se_stay, ymax =mean_stay + se_stay, group = session), alpha = 0.2) +
  # geom_ribbon(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), alpha = 0.3) +
  geom_line(data=data_diff_model, aes(colour = session)) + theme_Publication()  +   scale_color_manual(values=c('#999999','#E69F00'))+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks = element_blank(), strip.background=element_blank(), strip.text = element_blank())+ 
  xlab("Previous Points")+ ylab("P(stay) after \ndifferent first state")  + facet_wrap(~drug)  + coord_cartesian(ylim=c(0.20,1)) + 
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8)) +  geom_hline(yintercept =0.5, linetype = "dashed")


g_same_model <- ggplot(data=data_same, aes(x= prev_points, y = mean_stay, group = session)) + 
  geom_errorbar(aes(x = prev_points, y = mean_stay,ymin = mean_stay - se_stay, ymax =mean_stay + se_stay),colour = "black", width = 0, position = position_dodge(0.2))+
  geom_point(aes(x = prev_points, y = mean_stay, colour = session),position = position_dodge(0.2)) +
  geom_ribbon(data=data_same_model, aes(ymin = mean_stay - se_stay, ymax =mean_stay + se_stay, group = session), alpha = 0.2) +
  # geom_ribbon(data=data_same_model, aes(ymin = ci_l, ymax = ci_u), alpha = 0.3) +
  geom_line(data=data_same_model, aes(colour = session)) + theme_Publication()  +   scale_color_manual(values=c('#999999','#E69F00'))+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks = element_blank())+ 
  xlab("Previous Points")+ ylab("P(stay) after \nsame first state")  + facet_wrap(~drug)  + coord_cartesian(ylim=c(0.20,1)) + 
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8)) +  geom_hline(yintercept =0.5, linetype = "dashed")


(g_same_model / g_different_model)


## stats from model predictions -----
data_pred$stay_num <- as.numeric(data_pred$stay)

if (FALSE) {

  brm.simulated.behavior <- brm(stay_num|trials(1) ~ (session+prev_state_diff+prev_points|ID) + session*prev_points*prev_state_diff*ami_dummy+ 
                                  session*prev_points*prev_state_diff*nal_dummy,
                                data = data_pred, family = binomial(),
                                prior = c(set_prior("cauchy(0,2)", class = "sd"),
                                          set_prior("normal(0,3)", class = "b"),
                                          set_prior("lkj(2)", class = "cor")),
                                #set_prior("lkj(2)", class = "cor")),
                                warmup = 1000, iter =3000, chains =4,
                                control = list(adapt_delta = 0.99))
  }


# plot re-estimated model results

# summary(brm.simulated.behavior)

pars_set = c("b_prev_points", "b_prev_points:prev_state_diffdifferent", 
             "b_session2:prev_points:prev_state_diffdifferent:nal_dummyNal",
             "b_session2:prev_points:prev_state_diffdifferent:ami_dummyAmi")

probs = c(.5, .025, .975)

posterior <- as.matrix(brm.simulated.behavior)

library("bayesplot")

plot_title <- ggtitle("Posterior distributions", "with medians and 95% intervals")

# g_stat_plot <- mcmc_areas(posterior,
#                           pars = pars_set,
#                           prob = 0.95) + theme_Publication() #+ theme(# + 

g_stat_plot_model <- mcmc_intervals(posterior,prob = 0.8, prob_outer = 0.95,
                              pars = pars_set) + theme_Publication() #+ theme(# + 
g_stat_plot_model <- g_stat_plot_model + scale_y_discrete(name = "", limits = rev(pars_set), labels = c("Ami:Session:PrevPoints:PrevState",
                                                                                            "Nal:Session:PrevPoints:PrevState",
                                                                                            "PrevPoints:PrevState",
                                                                                            "PrevPoints")) + geom_vline(xintercept = 0, size =0.5, alpha= 0.5, linetype= 2)


g_stat_plot_model

## plot model predictions between sessions, 5 reward levels  -------------------------

data_pred$reward_level <- floor((data_pred$prev_points + 4)/2) -2 
pred_g_stay_session_diff_model <- data_pred%>%
  filter(trial!=1)%>%
  group_by(count_sim, session, reward_level, prev_state_diff, PredStay) %>%
  summarise(N=n(), ami_dummy = ami_dummy[1],
            nal_dummy = nal_dummy[1],
            drug = drug[1])  %>%
  mutate(freq = N/sum(N)) %>%
  filter(PredStay==1) %>%
  mutate(freq_diff = -10)

subjects = unique(pred_g_stay_session_diff_model$count_sim)

reward_levels = c(-2,-1,0,1,2)
for (n in subjects) {
  # print(n)
  for (r in reward_levels) {
    if (sum(pred_g_stay_session_diff_model$session[pred_g_stay_session_diff_model$count_sim ==n &
                                                   pred_g_stay_session_diff_model$reward_level ==r] == 1) ==2 &
        sum(pred_g_stay_session_diff_model$session[pred_g_stay_session_diff_model$count_sim ==n &
                                        pred_g_stay_session_diff_model$reward_level ==r] == 2) ==2) {
      
      pred_g_stay_session_diff_model$freq_diff[pred_g_stay_session_diff_model$session==2 & pred_g_stay_session_diff_model$count_sim ==n &
                                      pred_g_stay_session_diff_model$reward_level ==r] <- pred_g_stay_session_diff_model$freq[pred_g_stay_session_diff_model$session==2 & pred_g_stay_session_diff_model$count_sim ==n &
                                                                                                          pred_g_stay_session_diff_model$reward_level ==r] -
        pred_g_stay_session_diff_model$freq[pred_g_stay_session_diff_model$session==1 &
                                   pred_g_stay_session_diff_model$count_sim ==n &
                                   pred_g_stay_session_diff_model$reward_level ==r]
      
    } else
      cat("sub ", n, "and reward ", r, "\n")
    
    
  }
}

pred_g_stay_session_diff_model$reward_level <- as.factor(pred_g_stay_session_diff_model$reward_level )

g_model_diff_stay <- pred_g_stay_session_diff_model %>% 
  filter(session ==2, freq_diff !=-10, prev_state_diff=="different")  %>%
  group_by(drug, reward_level) %>% summarize(N=n(),
                                             mean_freq_diff = mean(freq_diff, na.rm = TRUE),
                                             se_freq_diff = sd(freq_diff, na.rm = TRUE)/sqrt(N))

g_beh_diff_stay$reward_level_n <- as.numeric(g_beh_diff_stay$reward_level) - 3
g_model_diff_stay$reward_level_n <- as.numeric(g_model_diff_stay$reward_level) - 3

g_model_different_sess_diff <- ggplot(g_beh_diff_stay, aes(x=reward_level_n, y = mean_freq_diff, fill = drug)) +
  geom_point(aes(colour = drug),stat = "identity", position = position_dodge(width = 0.1),  size = 3)+
  geom_hline(yintercept =0, linetype = "dashed")+
  geom_ribbon(data=g_model_diff_stay, aes(ymin = mean_freq_diff - se_freq_diff, ymax =mean_freq_diff +se_freq_diff), alpha = 0.2) +
  # geom_ribbon(data=data_same_model, aes(ymin = ci_l, ymax = ci_u), alpha = 0.3) +
  geom_line(data=g_model_diff_stay, aes(colour = drug)) + theme_Publication()  +#   scale_color_manual(values=c('#999999','#E69F00'))+
  
  geom_errorbar(aes(ymin= mean_freq_diff - se_freq_diff, ymax = mean_freq_diff +se_freq_diff), width = 0, position = position_dodge(0.9))+
  # scale_x_discrete(breaks = NULL) + #labels = c("-2"="-4","-1"= "","0"= "0", "1" = "", "2"="5"))+
  # scale_y_continuous(breaks = c(0,0.1,-0.1), limits= c(-0.15,0.15)) +
  facet_wrap(vars(drug)) +  ylab("\u0394 P(stay) after\ndifferent first state") +# labs(subtitle = "Different first state") +
  theme_Publication() + theme(legend.position = "none", strip.background=element_blank(), strip.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
  xlab("Previous Points") +    coord_cartesian(ylim = c(-0.1,0.15)) + scale_y_continuous(breaks = c(-0.1, 0.0, 0.1))

g_model_different_sess_diff




# (aes(x = drug, y= mean_par_t0, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3
g_model_same_stay <- pred_g_stay_session_diff_model %>% 
  filter(session ==2, freq_diff !=-10, prev_state_diff=="same")  %>%
  group_by(drug, reward_level) %>% summarize(N=n(),
                                             mean_freq_diff = mean(freq_diff, na.rm = TRUE),
                                             se_freq_diff = sd(freq_diff, na.rm = TRUE)/sqrt(N))

g_beh_same_stay$reward_level_n <- as.numeric(g_beh_same_stay$reward_level) - 3
g_model_same_stay$reward_level_n <- as.numeric(g_model_same_stay$reward_level) - 3

g_model_same_sess_diff <- ggplot(g_beh_same_stay, aes(x=reward_level_n, y = mean_freq_diff, fill = drug)) +
  geom_point(aes(colour = drug),stat = "identity", position = position_dodge(width = 0.1),  size = 3)+
  geom_hline(yintercept =0, linetype = "dashed")+
  geom_ribbon(data=g_model_same_stay, aes(ymin = mean_freq_diff - se_freq_diff, ymax =mean_freq_diff +se_freq_diff), alpha = 0.2) +
  # geom_ribbon(data=data_same_model, aes(ymin = ci_l, ymax = ci_u), alpha = 0.3) +
  geom_line(data=g_model_same_stay, aes(colour = drug)) + theme_Publication()  +#   scale_color_manual(values=c('#999999','#E69F00'))+
  
  geom_errorbar(aes(ymin= mean_freq_diff - se_freq_diff, ymax = mean_freq_diff +se_freq_diff), width = 0, position = position_dodge(0.9))+
  facet_wrap(vars(drug)) +  ylab("\u0394 P(stay) after\nsame first state") +# labs(subtitle = "Different first state") +
  theme_Publication() + theme(legend.position = "none",  axis.text.x = element_blank(), axis.ticks = element_blank()) +
  xlab("Previous Points") +    coord_cartesian(ylim = c(-0.1,0.15)) + scale_y_continuous(breaks = c(-0.1, 0.0, 0.1))

g_model_same_sess_diff

## model predictions supplementary figure 1 ----

plot_grid(plot_grid(g_same_model/g_different_model, g_model_same_sess_diff/g_model_different_sess_diff, nrow = 1),
          g_stat_plot_model + theme(axis.text = element_text(size = 10), panel.grid.major.y = element_line( size=.1, color="black", linetype = "dashed")),
          nrow = 2,
          rel_heights = c(1,0.3))




## correlation pars and earning ##################
g <- ggplot(data = data_group, aes(x=earn, y = w_rstan)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE) # + xlab("Earn")+ ylab("w") + 
theme(axis.text.x = element_text(face="bold", size=15),axis.text.y = element_text(face="bold", size=15) ,axis.title=element_text(size=30,face="bold"))
g

g <- ggplot(data = data_group, aes(x=earn, y = w_rstan)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE) # + xlab("Earn")+ ylab("w") + 
theme(axis.text.x = element_text(face="bold", size=15),axis.text.y = element_text(face="bold", size=15) ,axis.title=element_text(size=30,face="bold"))
g

data_corr<- tibble(w = c(data_group$w_rstan, data_group$w_t0_rstan),
                   ID= c(data_group$ID, data_group$ID),
                   earn = c(data_group$earn, data_group$earn_t0),
                   nal = c(factor(data_group$drug=="Nal", level=c(FALSE,TRUE), label =c("pla", "nal")), factor(rep(1, length(data_group$drug)), label = "pla")),
                   ami = c(factor(data_group$drug == "Ami", level= c(FALSE,TRUE), label = c("pla", "ami")), factor(rep(1, length(data_group$drug)), label = "pla")))

data_corr %>% View()         

options(mc.cores = 4)
data_corr$w_norm = (data_corr$w- mean(data_corr$w))/sd(data_corr$w)
data_corr$earn_norm = (data_corr$earn- mean(data_corr$earn))/sd(data_corr$earn)
mean(data_corr$w)
mod.corr <- brm(earn_norm ~ 1 + w_norm + (1|ID), 
                data = data_corr,
                warmup = 400, iter = 2000, chains = 4)
summary(mod.corr)

mod.corr_drug <- brm(earn_norm ~ 1 + nal + ami + (1|ID), 
                     data = data_corr,
                     warmup = 400, iter = 2000, chains = 4)
summary(mod.corr_drug)
g_earn <- ggplot(data = data_corr, aes(x= w, y = earn)) + 
  geom_point()+
  geom_smooth(method = lm, color = "red", se = TRUE) + 
  xlab("\u03C9") + ylab("earn") + theme_minimal(base_size = 25) +
  theme(axis.line = element_line(colour="black")) 




# theme(axis.text.x = element_text(face="bold", size=15),axis.text.y = element_text(face="bold", size=15) ,axis.title=element_text(size=30,face="bold"))



# lkj models correlations

L_omega_vect <- rstan::extract(fit_model, pars = "L_Omega")


## how does the model relate to staying behavior? ----

rand_eff <- fit_model_beh %>% ranef()
rand_eff <- rand_eff$ID # %>% glimpse
rand_eff %>% glimpse
data_group <- data_group[order(data_group$ID),]
data_group %>% View
cor.test(rand_eff[1:112,1,6] , data_group$sess_w_rstan)
cor.test(rand_eff[1:112,1,8] , data_group$sess_w_rstan)
cor.test(rand_eff[1:112,1,8] + rand_eff[1:112,1,6] , data_group$sess_w_rstan)

cor.test(rand_eff[1:112,1,6] , data_group$sess_beta1_rstan)
cor.test(rand_eff[1:112,1,8] , data_group$sess_beta1_rstan)
cor.test(rand_eff[1:112,1,8] + rand_eff[1:112,1,6] , data_group$sess_beta1_rstan)
df = tibble(slope_same = rand_eff[1:112,1,6] %>% ave(FUN = scale),
            slope_different = (rand_eff[1:112,1,8] +rand_eff[1:112,1,6]) %>% ave(FUN = scale),
            diff_in_slopes = rand_eff[1:112,1,8]%>% ave(FUN = scale),
            sess_beta1_rstan = data_group$sess_beta1_rstan%>% ave(FUN = scale),
            sess_w_rstan=data_group$sess_w_rstan%>% ave(FUN = scale),
            sess_g_rstan=data_group$sess_g_rstan%>% ave(FUN = scale)
)

mod1 <- lm(data = df, sess_beta1_rstan ~ slope_same +  diff_in_slopes)
summary(mod1)
mod1 <- lm(data = df, sess_w_rstan ~ slope_same +  diff_in_slopes)
summary(mod1)

mod1 <- lm(data = df, diff_in_slopes ~ sess_beta1_rstan + sess_w_rstan+ sess_g_rstan)
summary(mod1)
mod1 <- lm(data = df, slope_same ~ sess_beta1_rstan + sess_w_rstan+ sess_g_rstan)
summary(mod1)

df = tibble(slope_same = (rand_eff[1:112,1,6]+ rand_eff[1:112,1,4]) %>% ave(FUN = scale),
            slope_different = (rand_eff[1:112,1,8] + rand_eff[1:112,1,7]+rand_eff[1:112,1,6]+ rand_eff[1:112,1,4]) %>% ave(FUN = scale),
            diff_in_slopes = (rand_eff[1:112,1,8]+ rand_eff[1:112,1,7])%>% ave(FUN = scale),
            beta1_rstan = data_group$beta1_rstan%>% ave(FUN = scale),
            w_rstan=data_group$w_rstan%>% ave(FUN = scale),
            g_rstan=data_group$g_rstan%>% ave(FUN = scale)
)
mod1 <- lm(data = df, slope_same ~ beta1_rstan + w_rstan+ g_rstan)
summary(mod1)
mod1 <- lm(data = df, diff_in_slopes ~ beta1_rstan + w_rstan+ g_rstan)
summary(mod1)


## compare models ----------------------------------------------------------

fit_model_kool_2lr <- readRDS("Stan Model Results/M_kool_2lr.rds")
fit_model_kool_1lr <- readRDS("Stan Model Results/M_kool_1lr.rds") 

loo.fit_model <- loo::loo(fit_model)
loo.fit_model_kool <- loo::loo(fit_model_kool_1lr)
loo.fit_model_kool2 <- loo::loo(fit_model_kool_2lr)


pbma_BB_wts <- pseudobma_weights(cbind(loo.fit_model$pointwise[,"elpd_loo"],
                                       loo.fit_model_kool$pointwise[,"elpd_loo"],
                                       loo.fit_model_kool2$pointwise[,"elpd_loo"]))

cdata <- loo_compare(loo.fit_model, loo.fit_model_kool, loo.fit_model_kool2)

cData <- as.data.frame(cdata)
cData <- cData%>% mutate(model = c(1,2,3), BB_wts = pbma_BB_wts)
g_compare_models <- ggplot(cData, aes(x = model, y =  elpd_diff)) +  
  
  
  geom_errorbar(aes(ymin= elpd_diff - se_diff, ymax = elpd_diff+se_diff), width = 0.2, position = position_dodge(0.9)) + theme_Publication() +
  geom_bar(position = position_dodge(), stat = "identity") +  scale_x_discrete(name="", limits = c("M1", "M2", "M3")) + 
  
  ylab("Comparing expected\nlog predictive density")

g_compare_models


g_compare_models_BMA <- ggplot(cData, aes(x = model, y =  pbma_BB_wts)) +  
  geom_bar(position = position_dodge(), stat = "identity") +  scale_x_discrete(name="", limits = c("M1", "M2", "M3")) + theme_Publication() +
  
  
  #geom_errorbar(aes(ymin= elpd_diff - se_diff, ymax = elpd_diff+se_diff), width = 0, position = position_dodge(0.9)) + 
  ylab("Pseudo-BMA")
g_compare_models_BMA

plot_grid(g_compare_models,g_compare_models_BMA)


## serum model ----

fit_model_serum <- readRDS("Stan Model Results/M_new_serum.rds")

data_group_serum = data_group %>% filter(serum_pla != -1)
data_group_serum = data_group_serum[order(data_group_serum$s),] 
data_group_serum$beta1_t0_rstan <- get_posterior_mean(fit_model_serum, pars=c('beta1_t0'))[,5]
data_group_serum$w_t0_rstan <- get_posterior_mean(fit_model_serum, pars=c('w_t0'))[,5]
data_group_serum$g_t0_rstan <- get_posterior_mean(fit_model_serum, pars=c('g_t0'))[,5]

data_group_serum$beta1_rstan <- get_posterior_mean(fit_model_serum, pars=c('beta1'))[,5]
data_group_serum$w_rstan <- get_posterior_mean(fit_model_serum, pars=c('w'))[,5]
data_group_serum$g_rstan <- get_posterior_mean(fit_model_serum, pars=c('g'))[,5]

data_group_serum$sess_beta1_rstan <- get_posterior_mean(fit_model_serum, pars=c('sess_beta1'))[,5]
data_group_serum$sess_w_rstan <- get_posterior_mean(fit_model_serum, pars=c('sess_w'))[,5]
data_group_serum$sess_g_rstan <- get_posterior_mean(fit_model_serum, pars=c('sess_g'))[,5]

rel_height = 0.8
g_w_s  <- ggplot(data = data_group_serum %>% filter(serum_pla %in% c(0,1,2)) %>% mutate(serum_pla = as.factor(serum_pla)), aes(x = serum_pla, y = sess_w_rstan))+  # group = ID, linetype = Genotype)) 
  geom_boxplot(aes(fill = serum_pla, group = serum_pla), width=0.5, color="black", alpha=0.9, outlier.shape=NA)+
  geom_point(aes(fill = serum_pla), alpha = 0.3, position = position_jitterdodge(dodge.width= 0.9, jitter.width = jw_par), shape = 21, colour = "black", size = 2, stroke =1) +  
  theme_Publication()+
  scale_fill_manual(values = c("#619cff", "#aa3300","#ff3300"))+
  theme(legend.position = "none", 
        plot.subtitle = element_text(size = rel(rel_height), hjust = 0),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 15)) + 
  ylab(paste0("\u0394", "\U1D714", "'")) + labs(subtitle = "Model-based/model-free weight")+
  scale_x_discrete(labels =c("Pla","Ami\nlow\nserum", "Ami\nhigh\nserum" ))


g_b_s  <- ggplot(data = data_group_serum %>% filter(serum_pla %in% c(0,1,2)) %>% mutate(serum_pla = as.factor(serum_pla)), aes(x = serum_pla, y = sess_beta1_rstan))+  # group = ID, linetype = Genotype)) 
  geom_boxplot(aes(fill = serum_pla, group = serum_pla), width=0.5, color="black", alpha=0.9, outlier.shape=NA)+
  geom_point(aes(fill = serum_pla), alpha = 0.3, position = position_jitterdodge(dodge.width= 0.9, jitter.width = jw_par), shape = 21, colour = "black", size = 2, stroke =1) +  
  theme_Publication()+
  scale_fill_manual(values = c("#619cff", "#aa3300","#ff3300"))+
  theme(legend.position = "none", 
        plot.subtitle = element_text(size = rel(rel_height), hjust = 0),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 15)) + 
  ylab(paste0("\u0394", "\U1D702", "'"))  + labs(subtitle = "Inverse temperature")+ 
  scale_x_discrete(labels =c("Pla","Ami\nlow\nserum", "Ami\nhigh\nserum" ))

g_w_s + g_b_s




# glimpse results

print(comp_data)
stan_plot(fit_model_serum, pars_all)
pars_all <- grep("^beta", names(fit_model_serum), value = T)
pars_all <- pars_all[1:9]


Par_extract = rstan::extract(fit_model_serum, pars = c(pars_all, 'sigma'))

# in parameter estimation space:

w_ami_low_serum <- Par_extract$beta_ami
w_ami_low_serum %>% sf(1)

w_ami_high_serum <- Par_extract$beta_ami + Par_extract$beta_serum_ami_high
w_ami_high_serum %>% sf(1)


w_ami_diff_serum <- Par_extract$beta_serum_ami_high
w_ami_diff_serum%>% sf(1)


b_ami_low_serum <- Par_extract$beta_ami_noise
b_ami_low_serum %>% sf(1)

b_ami_high_serum <- Par_extract$beta_ami_noise  + Par_extract$beta_serum_ami_high_noise
b_ami_high_serum %>% sf(1)

b_ami_diff_serum <-  Par_extract$beta_serum_ami_high_noise
b_ami_diff_serum%>% sf(1)


# effect sizes  
sd_w_ses =  Par_extract$sigma[5]
sd_w =  Par_extract$sigma[2]

sd_b_ses =  Par_extract$sigma[4]
sd_b =  Par_extract$sigma[1]

sd_g_ses =  Par_extract$sigma[6]
sd_g =  Par_extract$sigma[3]

d_w_ami_low_serum <- (w_ami_low_serum/sqrt(sd_w_ses^2 + sd_w^2))  
d_w_ami_low_serum %>% sf(1)
d_w_ami_high_serum <- (w_ami_high_serum/sqrt(sd_w_ses^2 + sd_w^2))  
d_w_ami_high_serum %>% sf(1)
d_w_ami_diff_serum <- (w_ami_diff_serum/sqrt(sd_w_ses^2 + sd_w^2))  
d_w_ami_diff_serum%>% sf(1)
d_b_ami_low_serum <- (b_ami_low_serum/sqrt(sd_b_ses^2 + sd_b^2))  
d_b_ami_low_serum%>%  sf(1)
d_b_ami_high_serum <- (b_ami_high_serum/sqrt(sd_b_ses^2 + sd_b^2))  
d_b_ami_high_serum%>% sf(1)
d_b_ami_diff_serum <- (b_ami_diff_serum/sqrt(sd_b_ses^2 + sd_b^2))  
d_b_ami_diff_serum%>%  sf(1)


temp_mat <- bind_cols(B =  d_b_ami_diff_serum,
                      C =  d_b_ami_high_serum,
                      D =  d_b_ami_low_serum)


g_stats_serum_b <- temp_mat %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975),
            lls   = quantile(value, prob = .10),
            uls   = quantile(value, prob = .90)) %>%  # since the `key` variable is really two variables in one, here we split them up
  
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = name)) +
  geom_errorbar(aes(xmin = lls, xmax = uls), size = 1.5,color = "firebrick", width = 0)+
  geom_vline(xintercept = 0,  linetype = "dashed") +
  geom_pointrange(color = "firebrick") +
  labs( y = NULL) + # subtitle = "Effect sizes (95% and 50% quantiles)",
  theme_Publication(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +xlab("Effect sizes")+
  scale_y_discrete(labels = rev(c("Ami low serum \U1D702","Ami high serum \U1D702","Ami high - low serum \U1D702")))

temp_mat <- bind_cols(E =  d_w_ami_diff_serum,
                      F =  d_w_ami_high_serum,
                      G =  d_w_ami_low_serum)


g_stats_serum_w <- temp_mat %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975),
            lls   = quantile(value, prob = .10),
            uls   = quantile(value, prob = .90)) %>%  # since the `key` variable is really two variables in one, here we split them up
  
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = name)) +
  geom_errorbar(aes(xmin = lls, xmax = uls), size = 1.5,color = "firebrick", width = 0)+
  geom_vline(xintercept = 0,  linetype = "dashed") +
  geom_pointrange(color = "firebrick") +
  labs( y = NULL) + # subtitle = "Effect sizes (95% and 50% quantiles)",
  theme_Publication(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +xlab("Effect sizes")+
  scale_y_discrete(labels = rev(c("Ami low serum \U1D714","Ami high serum \U1D714","Ami high - low serum \U1D714")))


g_fig_5 <- plot_grid(g_w_s, g_b_s, g_stats_serum_w, g_stats_serum_b, ncol = 2 ,rel_heights = c(1,0.5), labels = c("a", "b"))




## model with stickiness parameters -----

# M1 model + stickiness parameters (with serum)
fit_model_sticky <- readRDS("Stan Model Results/M_new_sticky_serum.rds")

# compare m1 and m1-sticky ----
loo.fit_model_sticky <- loo::loo(fit_model_sticky)
loo.fit_model <- loo::loo(fit_model)
cdata <- loo_compare(loo.fit_model_sticky, loo.fit_model)

cData <- as.data.frame(cdata)
cData <- cData%>% mutate(model = c(1,2))
saveRDS(cData, file = "model_comparison_sticky.rds")


cData<- readRDS("model_comparison_sticky.rds")
g_compare_models <- ggplot(cData, aes(x = model, y =  elpd_diff)) +  
  
  
  geom_errorbar(aes(ymin= elpd_diff - se_diff, ymax = elpd_diff+se_diff), width = 0.2, position = position_dodge(0.9)) + theme_Publication() +
  geom_bar(position = position_dodge(), stat = "identity") +  scale_x_discrete(name="", limits = c("M1", "M1 - sticky")) + 
  
  ylab("Comparing expected\nlog predictive density")



fit_model <- readRDS("Stan Model Results/M_sticky_serum.rds")


Par_extract = rstan::extract(fit_model, pars = c(pars_all, 'sigma'))

# in parameter estimation space:

w_ami_low_serum <- Par_extract$beta_ami
w_ami_low_serum %>% sf(1)

w_ami_high_serum <- Par_extract$beta_ami + Par_extract$beta_serum_ami_high
w_ami_high_serum %>% sf(1)

w_ami_diff_serum <- Par_extract$beta_serum_ami_high 
w_ami_diff_serum %>% sf(1)

w_nal <- Par_extract$beta_nal 
w_nal %>% sf(1)

g_ami <- Par_extract$beta_ami_g
g_nal <- Par_extract$beta_nal_g

b_ami_low_serum_noise <- Par_extract$beta_ami_noise
b_ami_low_serum_noise %>% sf(1)

b_ami_high_serum_noise <- Par_extract$beta_ami_noise  + Par_extract$beta_serum_ami_high_noise
b_ami_high_serum_noise %>% sf(1)

b_ami_diff_serum <- Par_extract$beta_serum_ami_high_noise
b_ami_diff_serum%>% sf(1)

rho_ami_low_serum <- Par_extract$beta_ami_rho
rho_ami_low_serum_rho %>% sf(1)

rho_ami_high_serum <- Par_extract$beta_ami_rho  + Par_extract$beta_serum_ami_high_rho
rho_ami_high_serum %>% sf(1)

rho_ami_diff_serum <- Par_extract$beta_serum_ami_high_rho
rho_ami_diff_serum %>% sf(1)

pi_ami_low_serum <- Par_extract$beta_ami_pi
pi_ami_low_serum %>% sf(1)

pi_ami_high_serum <- Par_extract$beta_ami_pi  + Par_extract$beta_serum_ami_high_pi
pi_ami_high_serum %>% sf(1)

pi_ami_diff_serum <- Par_extract$beta_serum_ami_high_pi
pi_ami_diff_serum %>% sf(1)




b_nal <- Par_extract$beta_nal_noise

# effect sizes  
sd_w_ses =  Par_extract$sigma[5]
sd_w =  Par_extract$sigma[2]

sd_b_ses =  Par_extract$sigma[4]
sd_b =  Par_extract$sigma[1]

sd_g_ses =  Par_extract$sigma[6]
sd_g =  Par_extract$sigma[3]

sd_rho_ses =  Par_extract$sigma[7]
sd_rho =  Par_extract$sigma[8]

sd_pi_ses =  Par_extract$sigma[9]
sd_pi =  Par_extract$sigma[10]

d_w_ami_low_serum <- (w_ami_low_serum/sqrt(sd_w_ses^2 + sd_w^2))  
d_w_ami_low_serum %>% sf(1)
d_w_ami_high_serum <- (w_ami_high_serum/sqrt(sd_w_ses^2 + sd_w^2))  
d_w_ami_high_serum %>% sf(1)
d_w_ami_diff_serum <- (w_ami_diff_serum/sqrt(sd_w_ses^2 + sd_w^2))  
d_w_ami_diff_serum%>% sf(1)
d_b_ami_low_serum <- (b_ami_low_serum/sqrt(sd_b_ses^2 + sd_b^2))  
d_b_ami_low_serum%>%  sf(1)
d_b_ami_high_serum <- (b_ami_high_serum/sqrt(sd_b_ses^2 + sd_b^2))  
d_b_ami_high_serum%>% sf(1)
d_b_ami_diff_serum <- (b_ami_diff_serum/sqrt(sd_b_ses^2 + sd_b^2))  
d_b_ami_diff_serum%>%  sf(1)

d_rho_ami_low_serum <- (rho_ami_low_serum/sqrt(sd_rho_ses^2 + sd_b^2))  
d_rho_ami_low_serum%>%  sf(1)
d_rho_ami_high_serum <- (rho_ami_high_serum/sqrt(sd_rho_ses^2 + sd_b^2))  
d_rho_ami_high_serum%>% sf(1)
d_rho_ami_diff_serum <- (rho_ami_diff_serum/sqrt(sd_rho_ses^2 + sd_b^2))  
d_rho_ami_diff_serum%>%  sf(1)

d_pi_ami_low_serum <- (pi_ami_low_serum/sqrt(sd_pi_ses^2 + sd_b^2))  
d_pi_ami_low_serum%>%  sf(1)
d_pi_ami_high_serum <- (pi_ami_high_serum/sqrt(sd_pi_ses^2 + sd_b^2))  
d_pi_ami_high_serum%>% sf(1)
d_pi_ami_diff_serum <- (pi_ami_diff_serum/sqrt(sd_pi_ses^2 + sd_b^2))  
d_pi_ami_diff_serum%>%  sf(1)

# collect in a matrix, using letters to sort the columns as I want
temp_mat <- bind_cols(J =  d_w_ami_diff_serum,
                      K =  d_w_ami_high_serum,
                      L =  d_w_ami_low_serum,
                      G =  d_b_ami_diff_serum,
                      H =  d_b_ami_high_serum,
                      I =  d_b_ami_low_serum,
                      D =  d_rho_ami_diff_serum,
                      E =  d_rho_ami_high_serum,
                      F =  d_rho_ami_low_serum,
                      A =  d_pi_ami_diff_serum,
                      B =  d_pi_ami_high_serum,
                      C =  d_pi_ami_low_serum)


g_stats_serum_sticky <- temp_mat %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975),
            lls   = quantile(value, prob = .10),
            uls   = quantile(value, prob = .90)) %>%  # since the `key` variable is really two variables in one, here we split them up
  
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = name)) +
  geom_errorbar(aes(xmin = lls, xmax = uls), size = 1.5,color = "firebrick", width = 0)+
  geom_vline(xintercept = 0,  linetype = "dashed") +
  geom_pointrange(color = "firebrick") +
  labs( y = NULL) + # subtitle = "Effect sizes (95% and 50% quantiles)",
  theme_Publication(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +xlab("Effect sizes")+
  scale_y_discrete(labels = rev(c("Ami low serum \U1D714","Ami high serum \U1D714","Ami low - high serum \U1D714",
                                  "Ami low serum \U1D702","Ami high serum \U1D702","Ami low - high serum \U1D702",
                                  "Ami low serum \U1D70C","Ami high serum \U1D70C","Ami low - high serum \U1D70C",
                                  "Ami low serum \U1D70B","Ami high serum \U1D70B","Ami low - high serum \U1D70B")))


g_stats_serum_sticky




## mood, age, weight, serum, bmi stats -----------------------
data_group <- data_group %>% mutate(pos_mood_diff = PANAS_2_positive - PANAS_1_positive,
                                    neg_mood_diff = PANAS_2_negative - PANAS_1_negative)
data_group <- data_group %>% mutate(pos_mood_diff_s = pos_mood_diff %>% ave(FUN = scale),
                                    neg_mood_diff_s = neg_mood_diff %>% ave(FUN = scale))

if (FALSE) {
bmod_w_mood <- brm(sess_w_rstan ~ (ami_dummy + serum_ami_high + nal_dummy)*(Sex + Weight_s + Age_s)  + wm_s +  pos_mood_diff_s + neg_mood_diff_s+ PANAS_1_negative_s +PANAS_1_positive_s, 
                 data = data_group_serum %>% filter(serum_pla %in% c(0,1,2,-2)),
                 prior = c(set_prior("normal(0,1)", class = "b")),
                 warmup = 500, iter = 2000, chain = 4)

saveRDS(bmod_w_mood, file = "bmod_w_mood.rds")
bmod_w_mood %>%  fixef() %>% round(2) %>%  write.csv(file = "Beh_w_mood.csv")
}
bmod_w_mood <- readRDS("bmod_w_mood.rds")
post_mod <- posterior_samples(bmod_w_mood) #  pars = c("^b_")

bmod_w_mood %>% summary()

if (FALSE) {
bmod_eta_mood <- brm(sess_beta1_rstan ~  (ami_dummy + serum_ami_high + nal_dummy)*(Sex + Weight_s + Age_s)  + wm_s +  pos_mood_diff_s + neg_mood_diff_s+ PANAS_1_negative_s +PANAS_1_positive_s, 
                   data = data_group_serum %>% filter(serum_pla %in% c(0,1,2,-2)),
                   prior = c(set_prior("normal(0,1)", class = "b")),
                   warmup = 500, iter = 2000, chain = 4)

saveRDS(bmod_eta_mood, file = "bmod_eta_mood.rds")
bmod_eta_mood %>%  fixef() %>% round(2) %>%  write.csv(file = "Beh_eta_mood.csv")

}
bmod_eta_mood <- readRDS(file = "bmod_eta_mood.rds")
post_mod <- posterior_samples(bmod_eta_mood) #  pars = c("^b_")
bmod_eta_mood %>% summary()

summary(bmod_eta_mood)
if (FALSE) {
bmod_w_genetics <- brm(sess_w_rstan ~ (ami_dummy + serum_ami_high + nal_dummy)*(ankk_c + dat1_c + comt_s + darpp_c), 
                   data = data_group_serum %>% filter(serum_pla %in% c(0,1,2,-2)),
                   prior = c(set_prior("normal(0,1)", class = "b")),
                   warmup = 500, iter = 2000, chain = 4)

saveRDS(bmod_w_genetics, file = "bmod_w_genetics.rds")
bmod_w_genetics %>%  fixef() %>% round(2) %>%  write.csv(file = "bmod_w_genetics.csv")
}
bmod_w_genetics <- readRDS(file = "bmod_w_genetics.rds")
summary(bmod_w_genetics)
if (FALSE)  {
bmod_eta_genetics <- brm(sess_beta1_rstan ~ (ami_dummy + serum_ami_high + nal_dummy)*(ankk_c + dat1_c + comt_s + darpp_c), 
                       data = data_group_serum %>% filter(serum_pla %in% c(0,1,2,-2)),
                       prior = c(set_prior("normal(0,1)", class = "b")),
                       warmup = 500, iter = 2000, chain = 4)
saveRDS(bmod_eta_genetics, file = "bmod_eta_genetics.rds")
bmod_eta_genetics %>%  fixef() %>% round(2) %>%  write.csv(file = "bmod_eta_genetics.csv")

}

bmod_eta_genetics %>% summary()

bmod_eta_genetics <- readRDS(file = "bmod_eta_genetics.rds")
if (FALSE) {
bmod_mood_drugs <- brm(pos_mood_diff_s ~ (ami_dummy + serum_ami_high + nal_dummy), 
                         data = data_group_serum %>% filter(serum_pla %in% c(0,1,2,-2)),
                         prior = c(set_prior("normal(0,1)", class = "b")),
                         warmup = 500, iter = 2000, chain = 4)


saveRDS(bmod_mood_drugs, file = "bmod_mood_drugs.rds")
bmod_mood_drugs %>%  fixef() %>% round(2) %>%  write.csv(file = "bmod_mood.csv")
}
bmod_mood_drugs <- readRDS(file = "bmod_mood_drugs.rds")

if (FALSE) {
bmod_neg_mood_drugs <- brm(neg_mood_diff_s ~ (ami_dummy + serum_ami_high + nal_dummy), 
                       data = data_group_serum %>% filter(serum_pla %in% c(0,1,2,-2)),
                       prior = c(set_prior("normal(0,1)", class = "b")),
                       warmup = 500, iter = 2000, chain = 4)

saveRDS(bmod_neg_mood_drugs, file = "bmod_neg_mood_drugs.rds")
bmod_neg_mood_drugs %>%  fixef() %>% round(2) %>%  write.csv(file = "bmod_neg_mood.csv")
}
bmod_neg_mood_drugs <- readRDS(file = "bmod_neg_mood_drugs.rds")


# mood stats ------
# panas stats 

# dataset_chars %>% dplyr::select(starts_with("PA")) %>% round(1) %>% View()

dataset_panas_pos_t1<- data_group %>% dplyr::select(ID,drug, PANAS_1_positive) %>% mutate(time =1, PANAS_positive = PANAS_1_positive) %>% dplyr::select(-PANAS_1_positive)

dataset_panas_pos_t2<- data_group %>% dplyr::select(ID,drug, PANAS_2_positive) %>% mutate(time =2, PANAS_positive = PANAS_2_positive ) %>% dplyr::select(-PANAS_2_positive)
dataset_panas_neg_t1<- data_group %>% dplyr::select(ID,drug, PANAS_1_negative) %>% mutate(time =1, PANAS_negative = PANAS_1_negative) %>% dplyr::select(-PANAS_1_negative)
dataset_panas_neg_t2<- data_group %>% dplyr::select(ID,drug, PANAS_2_negative)%>% mutate(time =2, PANAS_negative = PANAS_2_negative) %>% dplyr::select(-PANAS_2_negative)

dataset_panas_pos <- rbind(dataset_panas_pos_t1, dataset_panas_pos_t2)
dataset_panas_pos %>% glimpse()
dataset_panas_neg <- rbind(dataset_panas_neg_t1, dataset_panas_neg_t2)
dataset_panas_neg %>% glimpse()

# model_panas_pos <-  lme(PANAS_positive ~ time*drug, random = ~time|ID, method = "ML", data = dataset_panas_pos, na.action = na.exclude)
# model_panas_pos %>% summary()
# stay ~ nal_dummy*prev_points*session*prev_state_diff+ ami_dummy*prev_points*session*prev_state_diff, random = ~ prev_state_diff + session + prevwin|ID, method = "ML",
#            data = dataset_panas_pos, na.action = na.exclude)
# if file.exists("Brms_model_panas_pos.rds")


if (FALSE) source("Utility scripts/Run_Models_with_Mood.r") # need this to save the models (will take some time)

brms_model_panas_neg <- readRDS(file = "brms_panas_neg.rds") 

brms_model_panas_pos <- readRDS(file ="brms_panas_pos.rds")
summary(brms_model_panas_pos)
summary(brms_model_panas_neg)
brms_model_panas_neg %>% fixef() %>% round(2) %>%  write.csv(file = "brms_model_panas_neg.csv")
brms_model_panas_pos %>% fixef() %>% round(2) %>%  write.csv(file = "brms_model_panas_pos.csv")

brms_model_panas_neg <- readRDS(file = "brms_panas_neg_c.rds") 

brms_model_panas_pos <- readRDS(file ="brms_panas_pos_c.rds")
summary(brms_model_panas_pos)
summary(brms_model_panas_neg)
brms_model_panas_neg %>% fixef() %>% round(2) %>%  write.csv(file = "brms_model_panas_neg.csv")
brms_model_panas_pos %>% fixef() %>% round(2) %>%  write.csv(file = "brms_model_panas_pos.csv")

# belief

data_group %>% filter(drug == "Ami") %>% dplyr::select(drug, Pill_Belief_Debriefing) %>% glimpse()
data_group <- data_group %>% mutate(placebo = (drug == "Pla"), placebo_belief = (Pill_Belief_Debriefing == 3)) 
data_group %>% group_by(drug, placebo_belief) %>% summarize(N=n())

# genetic distributions 
data_group %>% group_by(drug, comt) %>% summarize(N=n())
data_group %>% group_by(drug, ankk) %>% summarize(N=n())

data_group %>% group_by(drug, dat1) %>% summarize(N=n())

data_group %>% group_by(drug, darpp) %>% summarize(N=n())

## model parameters with working memory and mood ####
model_demographics_mood_wm <-  lm(sess_w_rstan ~ (nal_dummy + ami_dummy)*(PANAS_2_positive_s + PANAS_2_negative_s +PANAS_1_positive_s + PANAS_1_negative_s + wm_s + Age_s + BMI_new_s) + ami_dummy*(PANAS_2_positive_s + PANAS_2_negative_s +PANAS_1_positive_s + PANAS_1_negative_s + wm_s+ Age_s + BMI_new_s), data = data_group, na.action = na.exclude)
model_demographics_mood_wm <-  lm(sess_w_rstan ~ nal_dummy*(wm_s + Age_s + BMI_new_s) + ami_dummy*(wm_s+ Age_s + BMI_new_s), data = data_group, na.action = na.exclude)

model_demographics_mood_wm <-  lm(earn_diff ~ nal_dummy*(PANAS_2_positive_s + PANAS_2_negative_s +PANAS_1_positive_s + PANAS_1_negative_s + wm_s + Age_s + BMI_new_s) + ami_dummy*(PANAS_2_positive_s + PANAS_2_negative_s +PANAS_1_positive_s + PANAS_1_negative_s + wm_s+ Age_s + BMI_new_s), data = data_group, na.action = na.exclude)
model_demographics_mood_wm  %>% summary()
data_group <- data_group %>% mutate(earn_diff = earn - earn_t0)
data_group$PANAS_positive_sess_diff <- data_group$PANAS_2_positive - data_group$PANAS_1_positive
data_group$PANAS_negative_sess_diff <- data_group$PANAS_2_negative - data_group$PANAS_1_negative
data_group$PANAS_positive_sess_diff_s = ave(data_group$PANAS_positive_sess_diff, FUN = scale)
data_group$PANAS_negative_sess_diff_s = ave(data_group$PANAS_negative_sess_diff, FUN = scale)
w_mood_demo_model2 <- brm(sess_w_rstan ~ nal_dummy*(PANAS_negative_sess_diff_s + PANAS_positive_sess_diff_s + wm_s + Age_s + BMI_new_s) + ami_dummy*(PANAS_negative_sess_diff_s + PANAS_positive_sess_diff_s + wm_s+ Age_s + BMI_new_s), 
                          data = data_group,warmup = 400, iter = 2000, chains = 4)
summary(w_mood_demo_model2)
w_mood_demo_model <- brm(sess_w_rstan ~ nal_dummy*(PANAS_2_positive_s + PANAS_2_negative_s +PANAS_1_positive_s + PANAS_1_negative_s + wm_s + Age_s + BMI_new_s) + ami_dummy*(PANAS_2_positive_s + PANAS_2_negative_s +PANAS_1_positive_s + PANAS_1_negative_s + wm_s+ Age_s + BMI_new_s), 
                         data = data_group,warmup = 400, iter = 2000, chains = 4)
summary(w_mood_demo_model)
saveRDS(object = w_mood_demo_model2, file= "w_mood_demo_model2.rds")
saveRDS(object = w_mood_demo_model, file= "w_mood_demo_model.rds")
w_mood_demo_model %>% summary()

w_mood_demo_model2 %>% fixef() %>% round(3)  %>%   write.csv("w_mood_demo_model2.csv")
w_mood_demo_model <- readRDS("w_mood_demo_model2.rds")
w_mood_demo_model %>% summary()


## refitting model parameters ------------------
source("Util_scripts/refit_model.r") 
savedataset = "Refit_winning_model_pars_all.rds"
drf_temp <- readRDS(savedataset)
# drf_temp$om_mean <- 1/2*(drf_temp$om_good +drf_temp$om_bad)
# drf_temp$om_mean_rf <- 1/2*(drf_temp$om_good_rf +drf_temp$om_bad_rf)
drf_temp %>% glimpse()
# drf_temp$w_mean_rstan_rf - drf_temp$w_mean_rstan_rf
g_rf_w_mean<- ggplot(data =drf_temp, aes(x= w_mean_rstan , y = w_mean_rstan_rf)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
  theme_Publication() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = "none",
        aspect.ratio=1) +   ylab("refitted \u03C9'") + xlab("\u03C9'") + 
  labs(subtitle=paste("corr = ", round(cor.test(drf_temp$w_mean_rstan,drf_temp$w_mean_rstan_rf)[["estimate"]],2), sep=""))
# g_rf_w_mean

g_rf_beta1_mean<- ggplot(data =drf_temp, aes(x= beta1_mean_rstan , y = beta1_mean_rstan_rf)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
  theme_Publication() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = "none",
        aspect.ratio=1) +   ylab("refitted \u03B7	'") + xlab("\u03B7'") + 
  labs(subtitle=paste("corr = ", round(cor.test(drf_temp$beta1_mean_rstan,drf_temp$beta1_mean_rstan_rf)[["estimate"]],2), sep=""))
# g_rf_beta1_mean

g_rf_g_mean<- ggplot(data =drf_temp, aes(x= g_mean_rstan , y = g_mean_rstan_rf)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
  theme_Publication() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = "none",
        aspect.ratio=1) +   ylab("refitted \u0263'") + xlab("\u0263'") + 
  labs(subtitle=paste("corr = ", round(cor.test(drf_temp$g_mean_rstan,drf_temp$g_mean_rstan_rf)[["estimate"]],2), sep=""))
# g_rf_g_mean
plot_grid(g_rf_w_mean,g_rf_beta1_mean,g_rf_g_mean, nrow = 1)
# c("Ami \u03B2","Nal \u03B2","Ami \u0263","Nal \u0263","Ami \u03C9","Nal \u03C9"))+
# g_rf_om_good<- ggplot(data =drf_temp, aes(x= om_good, y = om_good_rf)) + 
#   geom_point()+
#   geom_smooth(method = "lm") + 
#   # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
#   theme_Publication() + 
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.grid.major  = element_blank(),
#         legend.position = "none") +   ylab("refitted om_good") + xlab("om_good") + 
#   labs(title=paste("corr = ", round(cor.test(drf_temp$om_good,drf_temp$om_good_rf)[["estimate"]],2), sep=""))

# g_rf_om_bad<- ggplot(data =drf_temp, aes(x= om_bad, y = om_bad_rf)) + 
#   geom_point()+
#   geom_smooth(method = "lm") + 
#   # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
#   theme_Publication() + 
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.grid.major  = element_blank(),
#         legend.position = "none") +   ylab("refitted om_bad") + xlab("om_bad") + 
#   labs(title=paste("corr = ", round(cor.test(drf_temp$om_bad,drf_temp$om_bad_rf)[["estimate"]],2), sep=""))

g_rf_noise<- ggplot(data =drf_temp, aes(x= noise, y = noise_rf)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
  theme_Publication() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = "none") +   ylab("refitted noise") + xlab("noise") + 
  labs(title=paste("corr = ", round(cor.test(drf_temp$noise,drf_temp$noise_rf)[["estimate"]],2), sep=""))
g_rf_mu0<- ggplot(data =drf_temp, aes(x= mu0, y = mu0_rf)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
  theme_Publication() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = "none") +   ylab("refitted mu0") + xlab("mu0") + 
  labs(title=paste("corr = ", round(cor.test(drf_temp$mu0,drf_temp$mu0_rf)[["estimate"]],2), sep=""))

g_rf_loggam<- ggplot(data =drf_temp, aes(x= loggam, y = loggam_rf)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  # coord_cartesian(xlim = c(-8,0), ylim = c(-8,0)) +   
  theme_Publication() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = "none") +   ylab("refitted log(gamma)") + xlab("log(gamma)") + 
  labs(title=paste("corr = ", round(cor.test(drf_temp$loggam,drf_temp$loggam_rf)[["estimate"]],2), sep=""))
plot_grid(g_rf_om_mean, g_rf_noise, g_rf_mu0,g_rf_loggam, ncol = 4)

# wm and drug effects #######################################################

data_group_wm <- data_group  %>%
  filter(!is.na(wm))
g2 <- ggplot(data = data_group_wm, aes(x = drug, y = wm))  # group = ID, linetype = Genotype)) 
g2 <- g2+          geom_violin(aes(fill = drug))
g2 <- g2+ geom_boxplot(aes(fill = drug), width=1, color="grey", alpha=0.5)
g2 <- g2+         geom_jitter() + theme(legend.position="none")
g2

if(!file.exists("brms_wm.rds")) {
  brms_wm <- brm(wm ~ nal_dummy + ami_dummy,
                 data = data_group_wm,
                 family = gaussian,
                 prior = c(set_prior("normal(0,3)", class = "b")),
                 warmup = 500, iter = 2000, chains = 4)
  saveRDS(brms_wm, file ="brms_wm.rds")
}
summary(brms_wm)
mcmc_plot(brms_wm)

#############################################################################################################
# Computational modelling with genetic data #################################################################
# get genetics model stats ------------------------------------------------

# note that these are not included to the same degree in the revised manuscript

if (FALSE) { # this will take some time
  run_model_fit(modelfile = "Stan Scripts/M_new_gen.stan", "Stan Model Results/M_new.rds") 
  data_temp_gen <- data_group  %>%
    filter(!is.na(dat1))
  # data_temp_gen %>% group_by(drug, comt) %>% summarize(N=n()) %>% View()
  
  data_temp_gen$beta1_t0_rstan <- get_posterior_mean(fit_model_gen, pars=c('beta1_t0'))[,5]
  data_temp_gen$w_t0_rstan <- get_posterior_mean(fit_model_gen, pars=c('w_t0'))[,5]
  data_temp_gen$g_t0_rstan <- get_posterior_mean(fit_model_gen, pars=c('g_t0'))[,5]
  
  data_temp_gen$beta1_rstan <- get_posterior_mean(fit_model_gen, pars=c('beta1'))[,5]
  data_temp_gen$w_rstan <- get_posterior_mean(fit_model_gen, pars=c('w'))[,5]
  data_temp_gen$g_rstan <- get_posterior_mean(fit_model_gen, pars=c('g'))[,5]
  
  data_temp_gen$sess_beta1_rstan <- get_posterior_mean(fit_model_gen, pars=c('sess_beta1'))[,5]
  data_temp_gen$sess_w_rstan <- get_posterior_mean(fit_model_gen, pars=c('sess_w'))[,5]
  data_temp_gen$sess_g_rstan <- get_posterior_mean(fit_model_gen, pars=c('sess_g'))[,5]
  
  saveRDS(data_temp_gen, file = "Data/data_group_gen.rds") 
  }
data_temp_gen <- readRDS(file = "Data/data_group_gen.rds") 






## plot the supplementary figure 2 #####


## a part #### 
if (!file.exists("GeneticEffectsBrms.rds")) {
  # contrasts(data_temp_gen$ankk)<- c(-0.5,0.5)
  # contrasts(data_temp_gen$dat1)<- c(-0.5,0.5)
  # contrasts(data_temp_gen$darpp)<- c(-0.5,0.5)
  data_temp_gen$comt_c <- data_temp_gen$comt %>% ave(FUN = scale)
  
  # data_temp_gen
  data_temp_gen <- data_temp_gen %>% mutate(sess_w_rstan_s  = ave(sess_w_rstan, FUN = scale))
  options(mc.cores = 4)
  data_temp_gen$serum_ami_low = data_temp_gen$serum_pla == 1
  gen_compare_brm <- brm(data = data_temp_gen, sess_w_rstan ~ (serum_ami_low + nal_dummy+ serum_ami_high)*(ankk_c + dat1_c + comt_c + darpp_c),
                         prior = c(set_prior("normal(0,1)", class = "b"),
                                   set_prior("cauchy(0,2)", class = "sigma")),
                         warmup = 1000, iter = 3000, chains = 4)
  
  gen_compare_brm_beta <- brm(data = data_temp_gen, sess_beta1_rstan ~ (serum_ami_low + nal_dummy+ serum_ami_high)*(ankk_c + dat1_c + comt_c + darpp_c),
                         prior = c(set_prior("normal(0,1)", class = "b"),
                                   set_prior("cauchy(0,2)", class = "sigma")),
                         warmup = 1000, iter = 3000, chains = 4)
  saveRDS(gen_compare_brm, file = "GeneticEffectsBrms_serum.rds")
  
} else {
  gen_compare_brm <- readRDS(file = "GeneticEffectsBrms.rds")
}
gen_compare_brm %>% summary
gen_compare_brm_beta  %>% summary
GeneticEffectsBrms %>% summary


names_order <- fixef(gen_compare_brm) %>% rownames()
names_order <- names_order[2:15]
names_order <- names_order[c(1,6,7,11,8,12,9,13,10,14,2:5)]

names_pars <- names_order %>% 
  gsub(pattern = "nal_dummyNal", replacement = "Nal") %>% 
  gsub(pattern = "ami_dummyAmi", replacement = "Ami") %>% 
  gsub(pattern = ":", replacement = "x")%>% 
  gsub(pattern = "comt_c", replacement = "COMT")%>% 
  gsub(pattern = "darpp1", replacement = "DARPP")%>% 
  gsub(pattern = "ankk1", replacement = "Taq1A")%>% 
  gsub(pattern = "dat11", replacement = "Dat")
color_scheme_set("red")
g_no_shrinkage <- mcmc_plot(gen_compare_brm, pars = "^b_", point_est = "mean",  prob = 0.8)  + labs(subtitle = "With Normal priors") +
  scale_y_discrete(name = "", limits = paste0('b_', names_order) %>% rev(), labels = names_pars %>% rev() )
g_no_shrinkage <-g_no_shrinkage + theme_Publication()  + geom_vline(xintercept=0) 
## b part ####

pars_all <- grep("^beta_", names(fit_model_gen), value = T)
pars_all<- pars_all[(!grepl("mix", pars_all))]
g_m_gen1 <- stan_plot(fit_model_gen, pars = pars_all, outer_level = 0.95, ci_level = 0.80, point_est = "mean", fill_color = "black", outline_color  = "black", est_color = "black") + theme_Publication()

g_m_gen<-  g_m_gen1 + geom_vline(xintercept=0) +  labs(subtitle = "With Shrinkage priors")+ theme_Publication() +
  scale_y_discrete(name = "", limits = c("DARPP", "COMT", "Dat1", "Taq1a", "Ami x DARPP","Nal x DARPP", "Ami x COMT", "Nal x COMT", 
                                         "Ami x Dat1", "Nal x Dat1", "Ami x Taq1a", "Nal x Taq1a", "Ami","Nal"))+
  scale_x_continuous(name = "")

g_m_gen <-  g_m_gen + theme(plot.title = element_blank())#+ 
# geom_rect(aes(xmin=-0.19,xmax=0.19,ymin=-Inf,ymax=Inf), colour = "gray", alpha = 0.01)#geom_ribbon(aes(xmin=-0.5,xmax=0.5), fill="blue", alpha=0.5)


sum_table_gen <- summary(fit_model_gen, pars = pars_all)
sum_table_gen$summary %>% round(3)%>%   write.csv("gen_results3.csv")




(g_no_shrinkage+ coord_cartesian(xlim = c(-1.3,1.3))) + (g_m_gen+ coord_cartesian(xlim = c(-1.3,1.3))) 

##


## report moderating effects of genotypes #####

# load posterior samples from both models:
pars_all <- grep("^beta_", names(fit_model_gen), value = T) # load again
pars_all<- pars_all[(!grepl("mix", pars_all))]
pars_sigma <- grep("^sigma", names(fit_model_gen), value = T)
pars_all = c(pars_all, pars_sigma)
Par_extract = rstan::extract(fit_model_gen, pars = pars_all)
sd_w_ses =  sqrt((Par_extract$`sigma[5]`)^2 + (Par_extract$`sigma[2]`)^2) # to calculate effect sizes


post_gen <- gen_compare_brm %>% posterior_samples()
sd_total <- post_gen$sigma 
post_gen <- post_gen/sd_total  # convert to effect size



# comt ami no shrinkage
(post_gen$`b_comt_c:ami_dummyAmi`) %>% sf(1)
# comt ami shrinkage
(Par_extract$beta_ami_comt/sd_w_ses) %>% sf(1)

# nal dat1 no shrinkage
(post_gen$`b_nal_dummyNal:dat11`) %>% sf(1)
# nal dat1 shrinkage
(Par_extract$beta_nal_dat/sd_w_ses) %>% sf(1)

# nal dat1 no shrinkage
(post_gen$`b_nal_dummyNal:darpp1`) %>% sf(1)
# nal dat1 shrinkage
(Par_extract$beta_nal_darpp/sd_w_ses) %>% sf(1)

## no shrinkage effect sizes ----
if (TRUE) { # no shrinkage
  # nal
  
  d_nal_ankk = (post_gen$`b_nal_dummyNal:ankk1`) 
  d_nal_ankk %>% sf(1)
  data_nal_ankk <- d_nal_ankk %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_nal_ankk_a1p = (post_gen$b_nal_dummyNal + 1/2*post_gen$`b_nal_dummyNal:ankk1`) 
  d_nal_ankk_a1p %>% sf(1)
  data_nal_ankk_a1p <- d_nal_ankk_a1p %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  
  d_nal_ankk_a1m <- (post_gen$b_nal_dummyNal - 1/2*post_gen$`b_nal_dummyNal:ankk1`) 
  # d_nal_ankk_a1m %>% sf()
  data_nal_ankk_a1m <- d_nal_ankk_a1m %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  # data_nal_ankk_a1m <- d_nal_ankk_a1p %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  d_nal_darpp <- (post_gen$`b_nal_dummyNal:darpp1`) 
  # d_nal_darpp %>% sf()
  data_nal_darpp <- d_nal_darpp %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_nal_darpp_highD1 <- (post_gen$b_nal_dummyNal + 1/2*post_gen$`b_nal_dummyNal:darpp1`) 
  # d_nal_darpp_highD1 %>% sf()
  data_nal_darpp_highD1 <- d_nal_darpp_highD1 %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_nal_darpp_lowD1<- (post_gen$b_nal_dummyNal - 1/2*post_gen$`b_nal_dummyNal:darpp1`) 
  # d_nal_darpp_lowD1%>% sf()
  data_nal_darpp_lowD1 <- d_nal_darpp_lowD1 %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_nal_dat <- (post_gen$`b_nal_dummyNal:dat11`) 
  # d_nal_dat_other %>% sf()
  data_nal_dat <- d_nal_dat %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  d_nal_dat_9r <- (post_gen$b_nal_dummyNal + 1/2*post_gen$`b_nal_dummyNal:dat11`) 
  # d_nal_dat_other %>% sf()
  data_nal_dat_9r <- d_nal_dat_9r %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_nal_dat_10 <- -(post_gen$b_nal_dummyNal - 1/2*post_gen$`b_nal_dummyNal:dat11`) 
  # d_nal_dat_9r%>% sf()
  data_nal_dat_10 <- d_nal_dat_10 %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  # comt nal
  d_nal_comt<- (post_gen$`b_nal_dummyNal:comt_c`) 
  data_nal_comt<- d_nal_comt%>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  # met met 
  d_nal_comt_MM <- (post_gen$b_nal_dummyNal + 1.23*post_gen$`b_nal_dummyNal:comt_c`) 
  data_nal_comt_MM <- d_nal_comt_MM %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  d_nal_comt_VM <- (post_gen$b_nal_dummyNal -0.12*post_gen$`b_nal_dummyNal:comt_c`) 
  data_nal_comt_VM <- d_nal_comt_VM %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  d_nal_comt_VV <- (post_gen$b_nal_dummyNal - 1.48*post_gen$`b_nal_dummyNal:comt_c`) 
  data_nal_comt_VV <- d_nal_comt_VV %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  # comt ami
  
  d_ami_comt <- (post_gen$`b_comt_c:ami_dummyAmi`) 
  # d_ami_comt %>% sf()
  data_ami_comt <- d_ami_comt %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  # met met
  d_ami_comt_MM <- (post_gen$b_ami_dummyAmi + 1.23*post_gen$`b_comt_c:ami_dummyAmi`) 
  # d_ami_comt_MM %>% sf()
  data_ami_comt_MM <- d_ami_comt_MM %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  # val val
  d_ami_comt_VM <-(post_gen$b_ami_dummyAmi + -0.12*post_gen$`b_comt_c:ami_dummyAmi`)
  # d_ami_comt_VM %>% sf()
  data_ami_comt_VM <- d_ami_comt_VM %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_comt_VV <-(post_gen$b_ami_dummyAmi + -1.48*post_gen$`b_comt_c:ami_dummyAmi`)
  # d_ami_comt_VV %>% sf()
  data_ami_comt_VV <- d_ami_comt_VV %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_ankk = (post_gen$`b_ankk1:ami_dummyAmi`) 
  # d_ami_ankk %>% sf() 
  data_ami_ankk <- d_ami_ankk %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_ankk_a1p = (post_gen$b_ami_dummyAmi + 1/2*post_gen$`b_ankk1:ami_dummyAmi`) 
  # d_ami_ankk_a1p %>% sf() 
  data_ami_ankk_a1p <- d_ami_ankk_a1p %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  
  d_ami_ankk_a1m <- (post_gen$b_ami_dummyAmi - 1/2*post_gen$`b_ankk1:ami_dummyAmi`) 
  # d_ami_ankk_a1m %>% sf()
  data_ami_ankk_a1m <- d_ami_ankk_a1m %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  # data_nal_ankk_a1m <- d_nal_ankk_a1p %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  d_ami_darpp <- (post_gen$`b_darpp1:ami_dummyAmi`) 
  # d_ami_darpp %>% sf()
  data_ami_darpp <- d_ami_darpp %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_darpp_highD1 <- (post_gen$b_ami_dummyAmi + 1/2*post_gen$`b_darpp1:ami_dummyAmi`) 
  # d_ami_darpp_highD1 %>% sf()
  data_ami_darpp_highD1 <- d_ami_darpp_highD1 %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_darpp_lowD1<- (post_gen$b_ami_dummyAmi - 1/2*post_gen$`b_darpp1:ami_dummyAmi`) 
  # d_ami_darpp_lowD1%>% sf()
  data_ami_darpp_lowD1 <- d_ami_darpp_lowD1 %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  d_ami_dat <- (post_gen$`b_dat11:ami_dummyAmi`) 
  # d_ami_dat_other %>% sf()
  data_ami_dat <- d_ami_dat %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_dat_9r <- (post_gen$b_ami_dummyAmi + 1/2*post_gen$`b_dat11:ami_dummyAmi`) 
  # d_ami_dat_other %>% sf()
  data_ami_dat_9r <- d_ami_dat_9r %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
  
  d_ami_dat_10 <- (post_gen$b_ami_dummyAmi - 1/2*post_gen$`b_dat11:ami_dummyAmi`) 
  # d_ami_dat_9r%>% sf()
  data_ami_dat_10 <- d_ami_dat_10 %>%  as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "no shrinkage")
  
}
## with shrinkage effect sizes ----
if (TRUE) {
  pars_sigma <- grep("^sigma", names(fit_model_gen), value = T)
  pars_all = c(pars_all, pars_sigma)
  Par_extract = rstan::extract(fit_model_gen, pars = pars_all)
  
  # turn it to effect sizes
  sd_w_ses =  sqrt((Par_extract$`sigma[5]`)^2 + (Par_extract$`sigma[2]`)^2)
  
  
  
  # data_temp_gen$comt_c %>% unique()
  # comt * ami
   s_ami_comt <- ((Par_extract$beta_ami - 0.13*Par_extract$beta_ami_comt)/sd_w_ses)  
  # s_ami_comt %>% sf()
  sata_ami_comt <- s_ami_comt  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_comt <- rbind(sata_ami_comt, data_ami_comt)
  
  
  
  s_ami_comt_MM <- ((Par_extract$beta_ami + 1.23*Par_extract$beta_ami_comt)/sd_w_ses)  
  # (s_ami_comt_MM - s_ami_comt_VV) %>% sf()
  # (1.23*Par_extract$beta_ami_comt ) %>% sf()
  # Par_extract$beta_ami%>% sf()
  
  sata_ami_comt_MM <- s_ami_comt_MM  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_comt_MM <- rbind(sata_ami_comt_MM, data_ami_comt_MM)
  
  
  
  s_ami_comt_VM <- ((Par_extract$beta_ami - 0.13*Par_extract$beta_ami_comt)/sd_w_ses)  
  # s_ami_comt_VM %>% sf()
  sata_ami_comt_VM <- s_ami_comt_VM  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_comt_VM <- rbind(sata_ami_comt_VM, data_ami_comt_VM)
  
  s_ami_comt_VV <- ((Par_extract$beta_ami - 1.48*Par_extract$beta_ami_comt)/sd_w_ses)  
  # s_ami_comt_VV %>% sf()
  sata_ami_comt_VV <- s_ami_comt_VV  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_comt_VV <- rbind(sata_ami_comt_VV, data_ami_comt_VV)
  
  
  # ankk * ami
  s_ami_ankk<- ((Par_extract$beta_ami_ankk)/sd_w_ses) 
  # s_ami_ankk%>% sf()
  sata_ami_ankk <- s_ami_ankk  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_ankk <- rbind(sata_ami_ankk, data_ami_ankk)
  
  
  
  s_ami_ankk_a1p<- ((Par_extract$beta_ami + 1/2*Par_extract$beta_ami_ankk)/sd_w_ses) 
  # s_ami_ankk_a1p%>% sf()
  sata_ami_ankk_a1p <- s_ami_ankk_a1p  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_ankk_a1p <- rbind(sata_ami_ankk_a1p, data_ami_ankk_a1p)
  
  s_ami_ankk_a1m <- ((Par_extract$beta_ami - 1/2*Par_extract$beta_ami_ankk)/sd_w_ses) 
  # s_ami_ankk_a1m %>% sf()
  sata_ami_ankk_a1m <- s_ami_ankk_a1m  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_ankk_a1m <- rbind(sata_ami_ankk_a1m, data_ami_ankk_a1m)
  
  # dat * ami 
  s_ami_dat <- ((Par_extract$beta_ami_dat)/sd_w_ses)  
  # s_ami_dat %>% sf()
  sata_ami_dat <- s_ami_dat  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_dat <- rbind(sata_ami_dat, data_ami_dat)
  
  
  s_ami_dat_9r <- ((Par_extract$beta_ami - 1/2*Par_extract$beta_ami_dat)/sd_w_ses)  
  # s_ami_dat_9r %>% sf()
  sata_ami_dat_9r <- s_ami_dat_9r  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_dat_9r <- rbind(sata_ami_dat_9r, data_ami_dat_9r)
  
  s_ami_dat_10 <- ((Par_extract$beta_ami + 1/2*Par_extract$beta_ami_dat)/sd_w_ses)  
  # s_ami_dat_10 %>% sf()
  sata_ami_dat_10 <- s_ami_dat_10  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_dat_10 <- rbind(sata_ami_dat_10, data_ami_dat_10)
  
  # darpp*ami
  s_ami_darpp <- ((Par_extract$beta_ami_darpp)/sd_w_ses)  
  # s_ami_darpp %>% sf()
  sata_ami_darpp <- s_ami_darpp  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_darpp <- rbind(sata_ami_darpp, data_ami_darpp)
  
  
  s_ami_darpp_l <- ((Par_extract$beta_ami + 1/2*Par_extract$beta_ami_darpp)/sd_w_ses)  
  # s_ami_darpp_l %>% sf()
  sata_ami_darpp_l <- s_ami_darpp_l  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_darpp_lowD1 <- rbind(sata_ami_darpp_l, data_ami_darpp_lowD1)
  
  s_ami_darpp_h <- ((Par_extract$beta_ami - 1/2*Par_extract$beta_ami_darpp)/sd_w_ses)  
  # s_ami_darpp_h %>% sf()
  sata_ami_darpp_h <- s_ami_darpp_h  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_ami_darpp_highD1 <- rbind(sata_ami_darpp_h, data_ami_darpp_highD1)
  
  
  # comt * nal
  s_nal_comt <- ((Par_extract$beta_nal_comt)/sd_w_ses)  
  # s_nal_comt %>% sf()
  sata_nal_comt <- s_nal_comt  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_comt <- rbind(sata_nal_comt, data_nal_comt)
  
  
  s_nal_comt_MM <- ((Par_extract$beta_nal + 1.23*Par_extract$beta_nal_comt)/sd_w_ses)  
  # s_nal_comt_MM %>% sf()
  sata_nal_comt_MM <- s_nal_comt_MM  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_comt_MM <- rbind(sata_nal_comt_MM, data_nal_comt_MM)
  
  
  s_nal_comt_VM <- ((Par_extract$beta_nal - 0.13*Par_extract$beta_nal_comt)/sd_w_ses)  
  # s_nal_comt_VM %>% sf()
  sata_nal_comt_VM <- s_nal_comt_VM  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_comt_VM <- rbind(sata_nal_comt_VM, data_nal_comt_VM)
  
  s_nal_comt_VV <- ((Par_extract$beta_nal - 1.48*Par_extract$beta_nal_comt)/sd_w_ses)  
  # s_ami_comt_VV %>% sf()
  sata_nal_comt_VV <- s_nal_comt_VV  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_comt_VV <- rbind(sata_nal_comt_VV, data_nal_comt_VV)
  
  # ankk * nal
  
  s_nal_ankk<- ((Par_extract$beta_nal_ankk)/sd_w_ses) 
  # s_nal_ankk %>% sf()
  sata_nal_ankk <- s_nal_ankk  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_ankk <- rbind(sata_nal_ankk, data_nal_ankk)
  
  
  s_nal_ankk_a1p<- ((Par_extract$beta_nal + 1/2*Par_extract$beta_nal_ankk)/sd_w_ses) 
  # s_nal_ankk_a1p %>% sf()
  sata_nal_ankk_a1p <- s_nal_ankk_a1p  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_ankk_a1p <- rbind(sata_nal_ankk_a1p, data_nal_ankk_a1p)
  
  
  # plot_effects(data_nal_ankk_a1p)
  
  s_nal_ankk_a1m <- ((Par_extract$beta_nal - 1/2*Par_extract$beta_nal_ankk)/sd_w_ses) 
  # s_nal_ankk_a1m %>% sf()
  sata_nal_ankk_a1m <- s_nal_ankk_a1m  %>% as.data.frame()  %>%  `colnames<-`("diff_w") %>% mutate(type = "with shrinkage")
  data_nal_ankk_a1m <- rbind(sata_nal_ankk_a1m, data_nal_ankk_a1m)
  
  
  # dat * nal 
  s_nal_dat <- ((Par_extract$beta_nal_dat)/sd_w_ses) 
  sata_nal_dat <- s_nal_dat  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_nal_dat <- rbind(sata_nal_dat, data_nal_dat)
  
  
  s_nal_dat_10 <- ((Par_extract$beta_nal + 1/2*Par_extract$beta_nal_dat)/sd_w_ses) 
  sata_nal_dat_10 <- s_nal_dat_10  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_nal_dat_10 <- rbind(sata_nal_dat_10, data_nal_dat_10)
  
  # s_nal_dat_10 %>% sf()
  s_nal_dat_9r <- ((Par_extract$beta_nal - 1/2*Par_extract$beta_nal_dat)/sd_w_ses)  
  # s_nal_dat_9r %>% sf()
  sata_nal_dat_9r <- s_nal_dat_9r  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_nal_dat_9r <- rbind(sata_nal_dat_9r, data_nal_dat_9r)
  
  
  # darpp*nal
  s_nal_darpp <- ((Par_extract$beta_nal_darpp)/sd_w_ses)  
  # s_nal_darpp %>% sf()
  sata_nal_darpp <- s_nal_darpp  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_nal_darpp<- rbind(sata_nal_darpp, data_nal_darpp)
  
  
  s_nal_darpp_h <- ((Par_extract$beta_nal + 1/2*Par_extract$beta_nal_darpp)/sd_w_ses)  
  # s_nal_darpp_h %>% sf()
  sata_nal_darpp_h <- s_nal_darpp_h  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_nal_darpp_highD1<- rbind(sata_nal_darpp_h, data_nal_darpp_highD1)
  
  
  
  s_nal_darpp_l <- ((Par_extract$beta_nal - 1/2*Par_extract$beta_nal_darpp)/sd_w_ses)  
  
  # s_nal_darpp_l %>% sf()
  
  sata_nal_darpp_lowD1 <- s_nal_darpp_l  %>% as.data.frame()  %>%  `colnames<-`("diff_w")   %>% mutate(type = "with shrinkage")
  data_nal_darpp_lowD1 <- rbind(sata_nal_darpp_lowD1, data_nal_darpp_lowD1)
  
}
## put together ----
if (TRUE) {
  # data_nal_ankk_a1p %>% glimpse()
 
  data_nal_ankk$level = NA
  data_nal_ankk_a1p$level = "a1+"
  data_nal_ankk_a1m$level = "a1-"
  data_nal_ankk <- rbind(data_nal_ankk, data_nal_ankk_a1m, data_nal_ankk_a1p)
  # data_nal_ankk$type %>% unique
  
  # data_nal_dat_10 %>% glimpse()
  data_nal_dat$level = NA
  data_nal_dat_10$level = "10/10"
  data_nal_dat_9r$level = "9 repeats"
  data_nal_dat <- rbind(data_nal_dat, data_nal_dat_9r, data_nal_dat_10)
  # data_nal_ankk %>% glimpse
  # plot_effect2(data_nal_dat)
  
  data_nal_darpp$level = NA
  data_nal_darpp_highD1$level = "T/T"
  data_nal_darpp_lowD1$level = "C/T&C/C"
  data_nal_darpp <- rbind(data_nal_darpp,data_nal_darpp_highD1, data_nal_darpp_lowD1)
  # plot_effect2(data_nal_darpp)
  
  data_ami_comt$level = NA
  data_ami_comt_MM$level = "Met/Met"
  data_ami_comt_VM$level = "Val/Met"
  data_ami_comt_VV$level = "Val/Val"
  data_ami_comt <- rbind(data_ami_comt, data_ami_comt_MM, data_ami_comt_VM, data_ami_comt_VV)
  # plot_effect2(data_ami_comt)
  # data_ami_ankk_a1p %>% glimpse()
  data_ami_ankk$level = NA
  data_ami_ankk_a1p$level = "a1+"
  data_ami_ankk_a1m$level = "a1-"
  data_ami_ankk <- rbind(data_ami_ankk,data_ami_ankk_a1m, data_ami_ankk_a1p)
  
  # plot_effect2(data_ami_ankk)
  # data_ami_ankk$type %>% unique
  
  # data_ami_dat_10 %>% glimpse()
  data_ami_dat$level = NA
  data_ami_dat_10$level = "10/10"
  data_ami_dat_9r$level = "9 repeats"
  data_ami_dat <- rbind(data_ami_dat,data_ami_dat_9r, data_ami_dat_10)
  # data_ami_ankk %>% glimpse
  # plot_effect2(data_ami_dat)
  data_ami_darpp$level = NA
  data_ami_darpp_highD1$level = "T/T"
  data_ami_darpp_lowD1$level = "C/T&C/C"
  data_ami_darpp <- rbind(data_ami_darpp,data_ami_darpp_highD1, data_ami_darpp_lowD1)
  # plot_effect2(data_ami_darpp)
  # data_ami_darpp %>% glimpse()
  data_nal_comt$level = NA
  data_nal_comt_MM$level = "Met/Met"
  data_nal_comt_VM$level = "Val/Met"
  data_nal_comt_VV$level = "Val/Val"
  data_nal_comt <- rbind(data_nal_comt,data_nal_comt_MM, data_nal_comt_VM, data_nal_comt_VV)
  # plot_effect2(data_nal_comt)
  
}


data_nal_darpp$genotype = "DARPP"
data_nal_ankk$genotype = "Taq1a"
data_nal_dat$genotype = "DAT"
data_nal_comt$genotype = "COMT"
data_nal_darpp$drug = "Nal"
data_nal_ankk$drug = "Nal"
data_nal_dat$drug = "Nal"
data_nal_comt$drug = "Nal"

data_ami_darpp$drug = "Ami"
data_ami_ankk$drug = "Ami"
data_ami_dat$drug = "Ami"
data_ami_comt$drug = "Ami"
data_ami_darpp$genotype = "DARPP"
data_ami_ankk$genotype = "Taq1a"
data_ami_dat$genotype = "DAT"
data_ami_comt$genotype = "COMT"
data_summary_gen <- rbind(data_nal_darpp , data_nal_ankk ,data_nal_dat, data_nal_comt,
                          data_ami_darpp,data_ami_ankk, data_ami_dat, data_ami_comt)
# data_summary_gen%>% glimpse()
data_nal_comt  %>% group_by(type, level)   %>%
  summarise(mean = quantile(diff_w, prob = .5) %>% round(3),
            ll   = quantile(diff_w, prob = .025)%>% round(3),
            ul   = quantile(diff_w, prob = .975)%>% round(3),
            p = 1- mean(diff_w>0) %>% round(3))

# data_nal 
data_summary_gen %>% filter(type == "no shrinkage") %>% group_by(drug,genotype, level)   %>%
  summarise(mean = quantile(diff_w, prob = .5) %>% round(3),
            ll   = quantile(diff_w, prob = .025)%>% round(3),
            ul   = quantile(diff_w, prob = .975)%>% round(3),
            p = 1- mean(diff_w>0) %>% round(3)) %>% write.csv("Model_with_shrinkage.csv")

# data_nal 
data_summary_gen %>% filter(type != "no shrinkage") %>% group_by(drug,genotype, level)   %>%
  summarise(mean = quantile(diff_w, prob = .5) %>% round(3),
            ll   = quantile(diff_w, prob = .025)%>% round(3),
            ul   = quantile(diff_w, prob = .975)%>% round(3),
            p = 1- mean(diff_w>0) %>% round(3)) %>% write.csv("Model_no_shrinkage.csv")

## genetic graphs (part c and d of sup fig 2)  -------------------


par ="w"
# "#F8766D" "#00BA38" "#619CFF"
par_rstan <- paste(par,"rstan",sep= "_")
par_t0_rstan <- paste(par,"t0_rstan",sep= "_")
par_sess <- paste("sess",par,"rstan", sep="_")
# 

par_rstan <- sym(par_rstan)
par_t0_rstan <- sym(par_t0_rstan)
par_sess <- sym(par_sess) 

levels(data_temp_gen$darpp) =  c("Low Str\nD1 eff.\n(C/C & C/T)", "High Str\nD1 eff.\n(T/T)")
levels(data_temp_gen$dat1) =  c("Low\nStr DA\n(10/10)", "High\nStr DA\n(9r carriers)")

levels(data_temp_gen$ankk) =  c("Low\nStrDA\n(a1-)", "High\nStr DA\n(a1+)")

levels(data_temp_gen$comt_f) =  c("Low\nPFC DA\n(Val/Val)", "\n\n(Met/Val)", "High\nPFC DA\n(Met/Met)")
#

plot_point_gen <- function(gen_par) {
  gen_par <- sym(gen_par)
  g <- data_temp_gen %>%
    group_by(drug, !!gen_par) %>%
    summarise(N = n(),
              mean_par = mean(!!par_rstan),
              mean_par_t0 = mean(!!par_t0_rstan),
              mean_par_sess = mean(!!par_sess),
              se_par = sd(!!par_rstan)/sqrt(N), 
              se_par_t0 = sd(!!par_t0_rstan)/sqrt(N),
              se_par_sess = sd(!!par_sess)/sqrt(N),
              max_par = max(c(!!par_rstan, !!par_t0_rstan)),
              min_par = min(c(!!par_rstan, !!par_t0_rstan))) %>%
    # View(data_barplot)
    # min_par_no <-min(min(data_barplot$min_par) - 0.1*abs(min(data_barplot$min_par)) ,0)
    # max_par_no <- max(max(data_barplot$max_par) + 0.1*abs(max(data_barplot$max_par)) ,1)
    
    # levels(data_barplot$ankk) =  c("Low\nStr DA\n(10/10)", "High\nStr DA\n(9r carriers)")
    # data_barplot %>% #filter(drug != "Ami") %>%
    ggplot(aes(x = drug, y= mean_par_sess)) +
    # geom_bar(position = position_dodge(), stat = "identity") +
    # geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    geom_errorbar(aes(ymin= mean_par_sess - se_par_sess, ymax = mean_par_sess+se_par_sess, group = drug), width = 0.2, position = position_dodge(0.9)) +
    geom_point(aes(x = drug, y= mean_par_sess, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    # geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_sess), size = 0.2, shape  = 3) +  theme(legend.position = "none") + 
    # ylim(min_par_no, max_par_no)  +
    ylab("\u0394 \u03C9'") +    facet_wrap(vars(!!gen_par))  +
    theme_Publication(base_size = 15)+
    theme(legend.position = "none", 
          axis.line = element_line(colour = "black"),
          plot.title = element_blank(),
          strip.background = element_blank()
    )
  return(g) }# + scale_colour_manual(values=c("#00BA38", "#619CFF"))+
g_point_ankk <- plot_point_gen("ankk") +  scale_x_discrete(name = "Taq1a",labels = NULL, limits = c("Nal", "Pla")) + coord_cartesian(ylim =c(-0.5,1))+ theme(axis.ticks.x = element_blank())
g_point_dat <- plot_point_gen("dat1") +  scale_x_discrete(name = "Dat1",labels = NULL,limits = c("Nal", "Pla"))+ theme(axis.title.y = element_blank())+ coord_cartesian(ylim =c(-0.5,1))+ theme(axis.ticks.x = element_blank())
g_point_darpp  <- plot_point_gen("darpp") +  scale_x_discrete(name = "DARPP-32",labels = NULL,limits = c("Nal", "Pla"))+ theme(axis.title.y = element_blank())+ coord_cartesian(ylim =c(-0.5,1))+ theme(axis.ticks.x = element_blank())

g_point_comt_nal <- plot_point_gen("comt_f") +   scale_x_discrete(name = "COMT",labels = NULL)+ theme(axis.ticks.x = element_blank())

g_point_comt <- plot_point_gen("comt_f") +   scale_x_discrete(name = "COMT",labels = NULL,limits = c("Ami", "Pla"))+ theme(axis.ticks.x = element_blank())



g_point_comt
g_legend <- get_legend(g_point_comt + 
                         theme(legend.position = "right",
                               legend.key = element_rect(colour = NA),
                               legend.direction = "horizontal",
                               legend.key.size= unit(0.2, "cm"),
                               # legend.margin = unit(0, "cm"),
                               legend.title = element_blank()))
g_legend2 <- get_legend(plot_effect2(data_nal_ankk) + 
                          theme(legend.position = "right",
                                legend.key = element_rect(colour = NA),
                                legend.direction = "horizontal",
                                legend.key.size= unit(0.2, "cm"),
                                # legend.margin = unit(0, "cm"),
                                legend.title = element_blank()))



g_m_gen
g_gen_ami <- plot_grid(g_point_comt,  
                       plot_effect2(data_ami_comt) , 
                       nrow = 2, rel_heights= c(1,0.3))
g2 <- plot_grid(g_point_ankk, g_point_dat, g_point_darpp, nrow = 1, rel_widths = c(1,1,1))
g_gen_nal <-plot_grid(g2,
                      plot_grid(plot_effect2(data_nal_ankk), plot_effect2(data_nal_dat),plot_effect2(data_nal_darpp), rel_widths = c(0.8,1,1), nrow = 1),
                      nrow = 2, rel_heights = c(1,0.3))
# g_gen_ami <- data
g_gen <- plot_grid(g_gen_nal,g_gen_ami, rel_widths = c(2,1))


plot_effect2(data_ami_comt)
plot_grid( g1, g2, nrow = 2, rel_heights = c(1,1))





