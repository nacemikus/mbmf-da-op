plot_bar_plots_amisul_nal <- function(par) {
  
  par_rstan <- paste(par,"rstan",sep= "_")
  par_t0_rstan <- paste(par,"t0_rstan",sep= "_")
  par_sess <- paste("sess",par,"rstan", sep="_")
  
  
  par_rstan <- sym(par_rstan)
  par_t0_rstan <- sym(par_t0_rstan)
  par_sess <- sym(par_sess) 
  
  
  
  data_barplot <- data_group %>% 
    group_by(drug) %>%
    summarise(N = n(),
              mean_par = mean(!!par_rstan),
              mean_par_t0 = mean(!!par_t0_rstan),
              se_par = sd(!!par_rstan)/sqrt(N), 
              se_par_t0 = sd(!!par_t0_rstan)/sqrt(N),
              max_par = max(c(!!par_rstan, !!par_t0_rstan)),
              min_par = min(c(!!par_rstan, !!par_t0_rstan)))
  
  min_par_no <-min(min(data_barplot$min_par) - 0.1*abs(min(data_barplot$min_par)) ,0)
  max_par_no <- max(max(data_barplot$max_par) + 0.1*abs(max(data_barplot$max_par)) ,1)
    
  
  
  g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug))+
    geom_bar(position = position_dodge(), stat = "identity") +
    # geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0, position = position_dodge(0.9)) +
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan), size = 0.2, shape  = 3) +  theme(legend.position = "none") + 
    ylab(par_rstan)  + ylim(min_par_no, max_par_no)
  
  g2 <- ggplot(data_barplot, aes(x = drug, y= mean_par_t0, fill = drug))+
    geom_bar(position = position_dodge(), stat = "identity") + 
    geom_errorbar(aes(ymin= mean_par_t0 - se_par_t0, ymax = mean_par_t0+se_par_t0), width = 0.2, position = position_dodge(0.9)) +
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_t0_rstan), size = 0.2, shape  = 3)  + theme(legend.position="none") + 
    ylab(par_t0_rstan)  +ylim(min_par_no, max_par_no)
  g <- ggplot(data = data_temp_gen, aes(x = drug, y = !!par_sess))+  # group = ID, linetype = Genotype)) 
    geom_violin(aes(fill = drug))+
    geom_boxplot(aes(fill = drug), width=1, color="grey", alpha=0.5)+
    geom_jitter(size = 0.1)
  plot_grid(g2,
            g1,
            g,
            label_x = 0.5,
            rel_widths = c(1,1,2),
            ncol = 3)
}



plot_bar_plots_amisul_nal_gen <- function(par, gen_par) {
  par_rstan <- paste(par,"rstan",sep= "_")
  par_t0_rstan <- paste(par,"t0_rstan",sep= "_")
  par_sess <- paste("sess",par,"rstan", sep="_")
  
  
  par_rstan <- sym(par_rstan)
  par_t0_rstan <- sym(par_t0_rstan)
  par_sess <- sym(par_sess) 
  gen_par <- sym(gen_par)
  
  data_barplot <- data_temp_gen %>% 
    group_by(drug, !!gen_par) %>%
    summarise(N = n(),
              mean_par = mean(!!par_rstan),
              mean_par_t0 = mean(!!par_t0_rstan),
              se_par = sd(!!par_rstan)/sqrt(N), 
              se_par_t0 = sd(!!par_t0_rstan)/sqrt(N))
  
  
  g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug))+
    geom_bar(position = position_dodge(), stat = "identity") + 
    geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0.2, position = position_dodge(0.9)) +
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan), size = 0.2, shape  = 3) +  theme(legend.position = "none") + 
    ylab(par_rstan)  + ylim(0,1) + facet_wrap(vars(!!gen_par))
  g2 <- ggplot(data_barplot, aes(x = drug, y= mean_par_t0, fill = drug))+
    geom_bar(position = position_dodge(), stat = "identity") + 
    geom_errorbar(aes(ymin= mean_par_t0 - se_par_t0, ymax = mean_par_t0+se_par_t0), width = 0.2, position = position_dodge(0.9)) +
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_t0_rstan), size = 0.2, shape  = 3)  + theme(legend.position="none") + 
    ylab(par_t0_rstan)  + ylim(0,1)  + facet_wrap(vars(!!gen_par))
  g <- ggplot(data = data_temp_gen, aes(x = drug, y = !!par_sess))+  # group = ID, linetype = Genotype)) 
    geom_violin(aes(fill = drug))+
    geom_boxplot(aes(fill = drug), width=1, color="grey", alpha=0.5)+
    geom_jitter(size = 0.1)  + facet_wrap(vars(!!gen_par))
  plot_grid(g2,
            g1,
            g,
            label_x = 0.5,
            rel_widths = c(1,1,2),
            ncol = 3)
  ### playaround! dont forget to comment 
  
  
  # g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug))+
  #   # geom_bar(position = position_dodge(), stat = "identity") + 
  #   
  #   geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0, position = position_dodge(0.9)) +
  #   geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
  #   geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan, colour = drug), size = 0.2, shape  = 3) + 
  #   ylab(par_rstan)  + ylim(min_par_no, max_par_no)
  # g1
  # 
  
}

plot_point_plots_amisul_nal <- function(par) {
  
  par_rstan <- paste(par,"rstan",sep= "_")
  par_t0_rstan <- paste(par,"t0_rstan",sep= "_")
  par_sess <- paste("sess",par,"rstan", sep="_")
  
  
  par_rstan <- sym(par_rstan)
  par_t0_rstan <- sym(par_t0_rstan)
  par_sess <- sym(par_sess) 
  
  
  
  data_barplot <- data_group %>% 
    group_by(drug) %>%
    summarise(N = n(),
              mean_par = mean(!!par_rstan),
              mean_par_t0 = mean(!!par_t0_rstan),
              se_par = sd(!!par_rstan)/sqrt(N), 
              se_par_t0 = sd(!!par_t0_rstan)/sqrt(N),
              max_par = max(c(!!par_rstan, !!par_t0_rstan)),
              min_par = min(c(!!par_rstan, !!par_t0_rstan)))
  
  min_par_no <-min(min(data_barplot$min_par) - 0.1*abs(min(data_barplot$min_par)) ,0)
  max_par_no <- max(max(data_barplot$max_par) + 0.1*abs(max(data_barplot$max_par)) ,1)
  
  
  
  # 
  
  g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug)) +
    # geom_bar(position = position_dodge(), stat = "identity") +
    # geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0.2, position = position_dodge(0.9)) +
    geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan), size = 0.2, shape  = 3) +  theme(legend.position = "none") + 
   ylim(min_par_no, max_par_no)  +
    ylab(par) + 
  theme_minimal(base_size = 25, )   + 
    theme(legend.position = "none", plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
          axis.line = element_line(colour = "black"))
  
  
    g2 <- ggplot(data_barplot, aes(x = drug, y= mean_par_t0, fill = drug))+
    # geom_bar(position = position_dodge(), stat = "identity") + 
    geom_errorbar(aes(ymin= mean_par_t0 - se_par_t0, ymax = mean_par_t0+se_par_t0), width = 0.2, position = position_dodge(0.9)) +
    geom_point(aes(x = drug, y= mean_par_t0, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
  
     geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_t0_rstan), size = 0.2, shape  = 3)  + theme(legend.position="none") + 
    ylab(par_t0_rstan)  +ylim(min_par_no, max_par_no)
  g <- ggplot(data = data_temp_gen, aes(x = drug, y = !!par_sess))+  # group = ID, linetype = Genotype)) 
    geom_violin(aes(fill = drug))+
    geom_boxplot(aes(fill = drug), width=1, color="grey", alpha=0.5)+
    geom_jitter(size = 0.1)
  plot_grid(g2,
            g1,
            g,
            label_x = 0.5,
            rel_widths = c(1,1,2),
            ncol = 3)
}



plot_point_plots_amisul_nal_gen <- function(par, gen_par) {
  par_rstan <- paste(par,"rstan",sep= "_")
  par_t0_rstan <- paste(par,"t0_rstan",sep= "_")
  par_sess <- paste("sess",par,"rstan", sep="_")
  
  
  par_rstan <- sym(par_rstan)
  par_t0_rstan <- sym(par_t0_rstan)
  par_sess <- sym(par_sess) 
  gen_par <- sym(gen_par)
  
  data_barplot <- data_temp_gen %>% 
    group_by(drug, !!gen_par) %>%
    summarise(N = n(),
              mean_par = mean(!!par_rstan),
              mean_par_t0 = mean(!!par_t0_rstan),
              se_par = sd(!!par_rstan)/sqrt(N), 
              se_par_t0 = sd(!!par_t0_rstan)/sqrt(N))
  # g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug))+
  #   # geom_bar(position = position_dodge(), stat = "identity") + 
  #   
  #   geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0, position = position_dodge(0.9)) +
  #   geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
  #   geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan, colour = drug), size = 0.2, shape  = 3) + 
  #   ylab(par_rstan)  + ylim(min_par_no, max_par_no)
  # g1
  
  g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug))+
    # geom_bar(position = position_dodge(), stat = "identity") + 
    geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0.2, position = position_dodge(0.9)) +
    geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan), size = 0.2, shape  = 3) +  theme(legend.position = "none") + 
    ylab(par_rstan)  + ylim(0,1) + facet_wrap(vars(!!gen_par))
  g2 <- ggplot(data_barplot, aes(x = drug, y= mean_par_t0, fill = drug))+
    # geom_bar(position = position_dodge(), stat = "identity") + 
    geom_errorbar(aes(ymin= mean_par_t0 - se_par_t0, ymax = mean_par_t0+se_par_t0), width = 0.2, position = position_dodge(0.9)) +
    geom_point(aes(x = drug, y= mean_par_t0, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
    geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_t0_rstan), size = 0.2, shape  = 3)  + theme(legend.position="none") + 
    ylab(par_t0_rstan)  + ylim(0,1)  + facet_wrap(vars(!!gen_par))
 
   g <- ggplot(data = data_temp_gen, aes(x = drug, y = !!par_sess))+  # group = ID, linetype = Genotype)) 
    geom_violin(aes(fill = drug))+
    geom_boxplot(aes(fill = drug), width=1, color="grey", alpha=0.5)+
    geom_jitter(size = 0.1)  + facet_wrap(vars(!!gen_par))
  plot_grid(g2,
            g1,
            g,
            label_x = 0.5,
            rel_widths = c(1,1,2),
            ncol = 3)
  
  
  ### playaround! dont forget to comment 
  
  
  # g1 <- ggplot(data_barplot, aes(x = drug, y= mean_par, fill = drug))+
  #   # geom_bar(position = position_dodge(), stat = "identity") + 
  #   
  #   geom_errorbar(aes(ymin= mean_par - se_par, ymax = mean_par+se_par), width = 0, position = position_dodge(0.9)) +
  #   geom_point(aes(x = drug, y= mean_par, colour = drug), position = position_dodge(width = 0.1), stat = "identity",  size = 3) +
  #   geom_jitter(data = data_temp_gen, aes(x=drug, y = !!par_rstan, colour = drug), size = 0.2, shape  = 3) + 
  #   ylab(par_rstan)  + ylim(min_par_no, max_par_no)
  # g1
  # 
  
}
