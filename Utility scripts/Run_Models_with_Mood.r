# load packages -----------------------------------------------------------





# load data ---------------------------------------------------------------


dataset <-readRDS("Data/data_mood.rds")


############## panas stats 
########################## 

dataset_panas_pos_t1<- dataset %>% dplyr::select(ID,ami_dummy, nal_dummy, PANAS_1_positive) %>% mutate(time =1, PANAS_positive = PANAS_1_positive) %>% dplyr::select(-PANAS_1_positive)

dataset_panas_pos_t2<- dataset %>% dplyr::select(ID,ami_dummy, nal_dummy, PANAS_2_positive) %>% mutate(time =2, PANAS_positive = PANAS_2_positive ) %>% dplyr::select(-PANAS_2_positive)
dataset_panas_neg_t1<- dataset %>% dplyr::select(ID,ami_dummy, nal_dummy, PANAS_1_negative) %>% mutate(time =1, PANAS_negative = PANAS_1_negative) %>% dplyr::select(-PANAS_1_negative)
dataset_panas_neg_t2<- dataset %>% dplyr::select(ID,ami_dummy, nal_dummy, PANAS_2_negative)%>% mutate(time =2, PANAS_negative = PANAS_2_negative) %>% dplyr::select(-PANAS_2_negative)

dataset_panas_pos <- rbind(dataset_panas_pos_t1, dataset_panas_pos_t2)
dataset_panas_pos$PANAS_positive_c <- (dataset_panas_pos$PANAS_positive - mean(dataset_panas_pos$PANAS_positive, na.rm = TRUE )) / sd(dataset_panas_pos$PANAS_positive, na.rm = TRUE) 
dataset_panas_pos$time <- dataset_panas_pos$time %>% as.factor()
# dataset_panas_pos %>% glimpse()
dataset_panas_neg <- rbind(dataset_panas_neg_t1, dataset_panas_neg_t2)
dataset_panas_neg$PANAS_negative_c <- (dataset_panas_neg$PANAS_negative - mean(dataset_panas_neg$PANAS_negative, na.rm = TRUE )) / sd(dataset_panas_neg$PANAS_negative, na.rm = TRUE) 

dataset_panas_neg$time <- dataset_panas_neg$time %>% as.factor()
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

if(!file.exists("brms_panas_pos.rds")) {
brms_model_panas_pos<- brm(PANAS_positive ~ time*(ami_dummy + serum_ami_high + nal_dummy)  + (1|ID),
                          data = dataset_panas_pos,
                          family = gaussian,
                          prior = c(set_prior("cauchy(0,2)", class = "sd"),
                                    set_prior("normal(0,3)", class = "b")),
                          warmup = 500, iter = 2000, chains = 4)
saveRDS(brms_model_panas_pos, file ="brms_panas_pos.rds")
}
if(!file.exists("brms_panas_neg.rds")){
brms_model_panas_neg <- brm(PANAS_negative ~ time*ami_dummy + time*nal_dummy + (1|ID),
                           data = dataset_panas_neg,family = gaussian,
                           prior = c(set_prior("cauchy(0,2)", class = "sd"),
                                     set_prior("normal(0,3)", class = "b")),
                           warmup = 500, iter = 2000, chains = 4)
saveRDS(brms_model_panas_neg, file = "brms_panas_neg.rds") 
}
if(!file.exists("brms_panas_pos_c.rds")) {
  brms_model_panas_pos_c<- brm(PANAS_positive_c ~ time*ami_dummy + time*nal_dummy + (1|ID),
                             data = dataset_panas_pos,
                             family = gaussian,
                             prior = c(set_prior("cauchy(0,2)", class = "sd"),
                                       set_prior("normal(0,3)", class = "b")),
                             warmup = 500, iter = 2000, chains = 4)
  saveRDS(brms_model_panas_pos_c, file ="brms_panas_pos_c.rds")
}
if(!file.exists("brms_panas_neg_c.rds")){
  brms_model_panas_neg_c <- brm(PANAS_negative_c ~ time*ami_dummy + time*nal_dummy + (1|ID),
                              data = dataset_panas_neg,family = gaussian,
                              prior = c(set_prior("cauchy(0,2)", class = "sd"),
                                        set_prior("normal(0,3)", class = "b")),
                              warmup = 500, iter = 2000, chains = 4)
  saveRDS(brms_model_panas_neg_c, file = "brms_panas_neg_c.rds") 
}
