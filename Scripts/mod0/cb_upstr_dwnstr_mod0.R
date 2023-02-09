# model 0: global model (non-hierarchical) --------------------------------
## specify data
mod0.dat<- list(log_RS=as.numeric(log(cb_upstr_dwnstr.dat$rs)),
                S=cb_upstr_dwnstr.dat$tot_spnrs,
                cov1=cb_upstr_dwnstr.dat$cov1,  #placeholder
                cov2=cb_upstr_dwnstr.dat$cov2,  #placeholder
                cov3=cb_upstr_dwnstr.dat$cov3,  #placeholder
                # basin=cb_upstr_dwnstr.dat$basin_index,
                nObs=length(cb_upstr_dwnstr.dat$rs))
                # nBasin=max(cb_upstr_dwnstr.dat$basin_index))

## specify model
cat('
  model {
  
  
  
  # seems like we should code this as a nested random effect (basin|popn) and generate predictions with one/that model to draw comparisons
  
  
  
    # Likelihood
    for (i in 1:nObs){
    # for (j in nBasin){
      
      # mu_basin[basin_vec[i]] ~ dnorm(0, taua)
      # mu_pop[pop_vec[i]] ~ dnorm(mu_basin[basin_vec[i]], taub_a)
      
      log_RS[i] ~ dnorm(mu_log_RS[i],tau)
      mu_log_RS[i] <- lnalpha - beta * S[i] + b1 * cov1[i] + b2 * cov2[i] + b3 * cov3[i]
      
      # + b1 * basin[]
      
      # + mu_pop[pop_vec[i]]
      
      ###  do we analyse all pops in one model or develop separate models for each? one model for all seems preferable
      ###  do we need a covariate to capture the effects of dam passage?
      ###  what other covariates do we need?
      ###  should we/can we include a random term?
      ###  smooth terms?
      ###  AR(IMA) terms?
      
     
      
      # # # res.law[i] <- sar.law[i] - mu.law[i]
      # # # sar.new[i] ~ dt(mu.law[i],tau.law,nu.law)
      # # # res.sar.new[i] <- sar.new[i] - mu.law[i]
      # #
      # # posterior predictive check
      # ## predicted values
      # law.pred[i]<-mu.law[i]
      # ## residuals for observed data
      # law.resid[i]<-sar.law[i]-law.pred[i]
      # ## discrepancy/squared residuals for observed data
      # law.SqResid[i]<-pow(law.resid[i],2)
      # 
      # # generate replicate data and compute fit stats
      # sar.new[i]~dt(mu.law[i],tau.law,nu.law)
      # # discrepancy/squared residuals for new/ideal data
      # law.SqResid.new[i]<-pow(sar.new[i]-law.pred[i],2)
    }
    # }
    
     alpha <- exp(lnalpha) #back-transformed alpha
    
    # Priors
    lnalpha ~ dunif(0,3) # prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
    beta ~ dunif(0,10) # prior for beta
    b1 ~ dnorm(0, 1) # prior for b1
    b2 ~ dnorm(0, 1) # prior for b2
    b3 ~ dnorm(0, 1) # prior for b3
    tau ~ dgamma(0.01, 0.01) # prior error
    
    # # derived values
    # ## sum of squared residuals for actual dataset
    # law.sumsqO<-sum(law.SqResid[])
    # ## sum of squared residuals for new dataset
    # law.sumsqN<-sum(law.SqResid.new[])
    # ## test whether new data are more extreme
    # law.test<-step(law.sumsqN-law.sumsqO)
    # ## bayesian pvalue
    # law.bpval<-mean(law.test)
    
    
  }', file={mod0.jags <- tempfile()})

## define parameters to monitor
mod0.params <- c("lnalpha", #log(alpha) term
                 "beta", #beta for spawners
                 "b1", #effect of cov1
                 "b2", #effect of cov2
                 "b3", #effect of cov3
                 "tau", #error term
                 "alpha") #back-transformed alpha

## call jags
fit.mod0 <- jags(data = mod0.dat,
                 parameters.to.save = mod0.params,
                 model.file = mod0.jags,
                 n.chains = 3,
                 n.iter = 250000,
                 n.burnin = 25000,
                 n.thin = 10,
                 # n.cluster = 3,
                 jags.seed = 123,
                 DIC = F)

## diagnostics
### waic and p_waic
#### generate samples
samples.mod0 <- jags.samples(fit.mod0$model, 
                             c("WAIC","deviance"), 
                             type = "mean", 
                             n.iter = 50000,
                             n.burnin = 5000,
                             n.thin = 1)

#### extract samples and calculate metrics
samples.mod0$p_waic <- samples.mod0$WAIC
samples.mod0$waic <- samples.mod0$deviance + samples.mod0$p_waic
tmp <- sapply(samples.mod0, sum)
waic.mod0 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)

### trace and density plots
#### extract simulations for plotting
mod0.mcmc <- as.mcmc(fit.mod0)
mod0.ggs.dat <- ggs(mod0.mcmc)

#### plots (these will have to be manipulated based on covariates selected)
##### MCMC (diagnostics)
###### trace
####### log(alpha)
mod0_lnalpha.trPlot <- ggs_traceplot(mod0.ggs.dat, family = "lnalpha")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "right",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "log(alpha)",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(mod0.mcmc,"saved")/post_dim(mod0.mcmc,"chains"),
           y = max(MCMCchains(mod0.mcmc, params = 'lnalpha')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.mod0$BUGSoutput$summary[6,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "serif",parse = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### beta
mod0_beta.trPlot <- ggs_traceplot(mod0.ggs.dat, family = "beta")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "right",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "beta",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(mod0.mcmc,"saved")/post_dim(mod0.mcmc,"chains"),
           y = max(MCMCchains(mod0.mcmc, params = 'beta')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.mod0$BUGSoutput$summary[5,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "serif",parse = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### b1
mod0_b1.trPlot <- ggs_traceplot(mod0.ggs.dat, family = "b1")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "right",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "b1",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(mod0.mcmc,"saved")/post_dim(mod0.mcmc,"chains"),
           y = max(MCMCchains(mod0.mcmc, params = 'b1')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.mod0$BUGSoutput$summary[2,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "serif",parse = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### b2
mod0_b2.trPlot <- ggs_traceplot(mod0.ggs.dat, family = "b2")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "right",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "b2",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(mod0.mcmc,"saved")/post_dim(mod0.mcmc,"chains"),
           y = max(MCMCchains(mod0.mcmc, params = 'b2')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.mod0$BUGSoutput$summary[3,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "serif",parse = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### b3
mod0_b3.trPlot <- ggs_traceplot(mod0.ggs.dat, family = "b3")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "right",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "b3",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(mod0.mcmc,"saved")/post_dim(mod0.mcmc,"chains"),
           y = max(MCMCchains(mod0.mcmc, params = 'b3')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.mod0$BUGSoutput$summary[4,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "serif",parse = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### tau
mod0_tau.trPlot <- ggs_traceplot(mod0.ggs.dat, family = "tau")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "right",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "tau",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(mod0.mcmc,"saved")/post_dim(mod0.mcmc,"chains"),
           y = max(MCMCchains(mod0.mcmc, params = 'tau')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.mod0$BUGSoutput$summary[7,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "serif",parse = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

###### density
####### log(alpha)
mod0_lnalpha.densPlot <- ggs_density(mod0.ggs.dat, family = "lnalpha")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "",y = "Density", x = "Value") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### beta
mod0_beta.densPlot <- ggs_density(mod0.ggs.dat, family = "beta")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "beta",y = "Density", x = "Value") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### b1
mod0_b1.densPlot <- ggs_density(mod0.ggs.dat, family = "b1")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "",y = "Density", x = "Value") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### b2
mod0_b2.densPlot <- ggs_density(mod0.ggs.dat, family = "b2")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "",y = "Density", x = "Value") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### b3
mod0_b3.densPlot <- ggs_density(mod0.ggs.dat, family = "b3")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "b3",y = "Density", x = "Value") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

####### tau
mod0_tau.densPlot <- ggs_density(mod0.ggs.dat, family = "tau")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "serif"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "serif"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "",y = "Density", x = "Value") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif"))

###### combined plots
####### log(alpha)
mod0_lnalpha.combPlot<-ggarrange(mod0_lnalpha.trPlot,
                                 mod0_lnalpha.densPlot,
                                 ncol = 1,
                                 nrow = 2)

png(paste("Output\\Figures\\Diagnostic\\MCMC\\mod0\\mod0_lnalpha.comb_tr_dens_plot",".png",sep=""),
    type="cairo",
    units="in",
    width=9,
    height=12,
    res=300)

print(mod0_lnalpha.combPlot)
dev.off()

####### beta
mod0_beta.combPlot<-ggarrange(mod0_beta.trPlot,
                              mod0_beta.densPlot,
                              ncol = 1,
                              nrow = 2)

png(paste("Output\\Figures\\Diagnostic\\MCMC\\mod0\\mod0_beta.comb_tr_dens_plot",".png",sep=""),
    type="cairo",
    units="in",
    width=9,
    height=12,
    res=300)

print(mod0_beta.combPlot)
dev.off()

####### b1
mod0_b1.combPlot<-ggarrange(mod0_b1.trPlot,
                            mod0_b1.densPlot,
                            ncol = 1,
                            nrow = 2)

png(paste("Output\\Figures\\Diagnostic\\MCMC\\mod0\\mod0_b1.comb_tr_dens_plot",".png",sep=""),
    type="cairo",
    units="in",
    width=9,
    height=12,
    res=300)

print(mod0_b1.combPlot)
dev.off()

####### b2
mod0_b2.combPlot<-ggarrange(mod0_b2.trPlot,
                            mod0_b2.densPlot,
                            ncol = 1,
                            nrow = 2)

png(paste("Output\\Figures\\Diagnostic\\MCMC\\mod0\\mod0_b2.comb_tr_dens_plot",".png",sep=""),
    type="cairo",
    units="in",
    width=9,
    height=12,
    res=300)

print(mod0_b2.combPlot)
dev.off()

####### b3
mod0_b3.combPlot<-ggarrange(mod0_b3.trPlot,
                            mod0_b3.densPlot,
                            ncol = 1,
                            nrow = 2)

png(paste("Output\\Figures\\Diagnostic\\MCMC\\mod0\\mod0_b3.comb_tr_dens_plot",".png",sep=""),
    type="cairo",
    units="in",
    width=9,
    height=12,
    res=300)

print(mod0_b3.combPlot)
dev.off()

####### tau
mod0_tau.combPlot<-ggarrange(mod0_tau.trPlot,
                             mod0_tau.densPlot,
                             ncol = 1,
                             nrow = 2)

png(paste("Output\\Figures\\Diagnostic\\MCMC\\mod0\\mod0_tau.comb_tr_dens_plot",".png",sep=""),
    type="cairo",
    units="in",
    width=9,
    height=12,
    res=300)

print(mod0_tau.combPlot)
dev.off()

###### auto-correlation
update(fit.mod0, 10000)
####### log(alpha)
post.lnalpha <- jags.samples(fit.mod0,
                     variable.names=c("lnalpha"),
                     n.iter=20000)


acf(fit.mod0$BUGSoutput$sims.array[,1])

str(fit.mod0)

##### model output (predictions, etc.)
###### pred. vs. estms.
####### generate predictions
######## isolate simulations matrix
mod0.mcmc = fit.mod0$BUGSoutput$sims.matrix

######## isolate covariate values to generate predictions 
mod0.newdata = data.frame(S = mod0.dat$S,
                                    cov1 = mod0.dat$cov1,
                                    cov2 = mod0.dat$cov2,
                                    cov3 = mod0.dat$cov3)

######## generate model matrix
mod0.Xmat = model.matrix(~S+cov1+cov2+cov3, mod0.newdata)[,2:5]

######## isolate predictors
mod0.coefs = mod0.mcmc[, c("beta",
                      "b1",
                      "b2",
                      "b3")]

######## simulate predictions
mod0.fit.pred = mod0.coefs %*% t(mod0.Xmat)

######## consolidate covariate values and predictions (including HPD intervals)
mod0.newdata = mod0.newdata %>% cbind(tidyMCMC(mod0.fit.pred, conf.int = TRUE, conf.method = "HPDinterval"))

######## manipulate data for plotting
mod0.plotData <- data.frame(brd_yr = cb_upstr_dwnstr.dat$brd_yr,
                            lnrs = log(cb_upstr_dwnstr.dat$rs),
                            mod0.newdata)

######## generate plot
## generate chapter plot
mod0.plot_insamp<-ggplot() +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = "serif"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "serif"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "serif"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "serif"),
        legend.title = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(size=12),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title ="model 0: global model (non-hierarchical)", y = bquote(bold('log'[e]*"(Recruits/Spawner)")), x = "Brood Year") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "serif")) +
  geom_ribbon(data = mod0.plotData,aes(x = brd_yr,ymin = conf.low,ymax = conf.high),alpha=0.3,fill = "gray")+
  geom_line(data = mod0.plotData,aes(brd_yr, estimate), size = 1.0,color = "black")+
  geom_point(data = mod0.plotData,aes(brd_yr,lnrs),shape = 21,size = 3.5,stroke=0.5)+
  geom_hline(yintercept = 0,linetype = 2)+
  scale_y_continuous(limits=c(-ceiling(max(abs(max(mod0.plotData$conf.low)),abs(max(mod0.plotData$conf.high)))),ceiling(max(abs(max(mod0.plotData$conf.low)),abs(max(mod0.plotData$conf.high))))),
                     breaks = seq(-ceiling(max(abs(max(mod0.plotData$conf.low)),abs(max(mod0.plotData$conf.high)))),ceiling(max(abs(max(mod0.plotData$conf.low)),abs(max(mod0.plotData$conf.high)))),2.0))+
  scale_x_continuous(limits=c(min(mod0.plotData$brd_yr),max(mod0.plotData$brd_yr)),breaks = seq(min(mod0.plotData$brd_yr),max(mod0.plotData$brd_yr),3))

## output chapter plot
### set image output parameters
png(filename="Output\\Figures\\Fit\\Pred v Estms\\mod0\\mod0_plot_insamp.png",
    type="cairo",
    units="in",
    width=9,
    height=6,
    res=300)

### write object to working directory
print(mod0.plot_insamp)
dev.off()