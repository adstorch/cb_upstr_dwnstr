# model 1: global model ---------------------------------------------------
## specify data
logRS_0.dat<- list(log_RS=as.numeric(log(cb_upstr_dwnstr.dat$rs)),
                   S=cb_upstr_dwnstr.dat$tot_spnrs,
                   basin=cb_upstr_dwnstr.dat$basin_index,
                   nObs=length(cb_upstr_dwnstr.dat$rs),
                   nBasin=max(cb_upstr_dwnstr.dat$basin_index))

## specify model
cat('
  model {
    # Likelihood
    for (i in 1:nObs){
    for (j in nBasin){
      
      # mu_basin[basin_vec[i]] ~ dnorm(0, taua)
      # mu_pop[pop_vec[i]] ~ dnorm(mu_basin[basin_vec[i]], taub_a)
      
      log_RS[i,j] ~ dnorm(mu_log_RS[i,j],tau)
      mu_log_RS[i,j] <- lnalpha[j] - beta * S[i,j]
      
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
    }
    
     alpha[j] <- exp(lnalpha)
    
    # Priors
    lnalpha ~ dunif(0,3) # prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
    beta ~ dunif(0,10) # prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
    b1 ~ dnorm(0, 1) # prior for effect of ocean survival
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
    
    
  }', file={logRS_0.jags <- tempfile()})

## define parameters to monitor
logRS_0.params <- c("lnalpha", # log(alpha) term
                    "beta", # beta for wild spawners
                    # "b1", # effect of ocean survival 
                    "tau",
                    "alpha") # error term)

## call jags
fit.logRS_0 <- jags.parallel(data = logRS_0.dat,
                             parameters.to.save = logRS_0.params,
                             model.file = logRS_0.jags,
                             n.chains = 3,
                             n.iter = 250000,
                             n.burnin = 25000,
                             n.thin = 10,
                             n.cluster = 3,
                             jags.seed = 123,
                             DIC = F)

## extract log-likelihood and calculate waic score
logRS_0.paramlist <- fit.logRS_0$BUGSoutput$sims.list
logRS_0.loglik <- logRS_0.paramlist$loglik
logRS_0.waic <- waic(logRS_0.loglik)

## diagnostics
### extract simulations
logRS_0.mcmc <- as.mcmc(fit.logRS_0)
logRS_0.ggs.dat <- ggs(logRS_0.mcmc)

### trace plots
#### log(alpha)
logRS_0_lnalpha.trPlot <- ggs_traceplot(logRS_0.ggs.dat, family = "lnalpha")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 50, b = 0, l = 0),family = "Calibri"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "Calibri"),
        axis.text.x = element_text(face = "bold",size = 14,color="black", vjust=0.5,family = "Calibri"),
        axis.text.y = element_text(face = "bold",size = 14,color="black",family = "Calibri"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color="black",family = "Calibri"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(face = "bold",size = 12,color="black",family = "Calibri"),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title = "log(alpha)",y = "Value", x = "Iteration") +
  annotate(geom = "text",
           x = post_dim(logRS_0.mcmc,"saved")/post_dim(logRS_0.mcmc,"chains"),
           y = max(MCMCchains(logRS_0.mcmc, params = 'lnalpha')),
           label = paste("hat(R)","~`=`~",round(as.numeric(fit.logRS_0$BUGSoutput$summary[1,8]),3),sep=""),
           hjust = 1.0,vjust=0.0,size = 5.1,family = "Calibri",parse = TRUE)+ # Rhat has to be Changed based on jags output (extraction rounds to nearest interger)
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "Calibri"))

### combined plots



### density plots
