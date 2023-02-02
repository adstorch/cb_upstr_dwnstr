### specify model
cat('
  model {
    # Likelihood
    for (i in 1:n){
      pp_log_RS[i] ~ dnorm(mu_log_RS[i],tau)
      mu_log_RS[i] <- lnalpha - betaW * Sw[i] - betaH * Sh[i] + b1 * ocean_surv[i] + b2 * basin[i]
      ###  do we analyse all pops in one model or develop separate models for each? one model for all seems preferable
      ###  do we need a covariate to capture the effects of dam passage?
      ###  what other covariates do we need?
      ###  should we/can we include a random term?
      ###  smooth terms?
      
      alpha <- exp(lnalpha)

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
    
    # Priors
    lnalpha ~ dunif(0,3) # prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
    betaW ~ dunif(0,10) # prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
    betaH ~ dunif(0,10) # prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
    b1 ~ dnorm(0, 1) # prior for effect of ocean survival
    tau ~ dgamma(0.01, 0.01) # prior error
    
    # derived values
    ## sum of squared residuals for actual dataset
    law.sumsqO<-sum(law.SqResid[])
    ## sum of squared residuals for new dataset
    law.sumsqN<-sum(law.SqResid.new[])
    ## test whether new data are more extreme
    law.test<-step(law.sumsqN-law.sumsqO)
    ## bayesian pvalue
    law.bpval<-mean(law.test)
    
    
  }', file={logRS_0.jags <- tempfile()})

### define parameters to monitor
params.logRS_0 <- c(lnalpha, # log(alpha) term
                    betaW, # beta for wild spawners
                    betaH, # beta for hatchery spawners
                    b1, # effect of ocean survival 
                    tau) # error term)

### call jags
fit.logRS_0 <- jags(data = cbpSARequiv.lawDat,
                    inits = inits.law,
                    parameters.to.save = params.law,
                    model.file = logRS_0.jags,
                    n.chains = 3,
                    n.iter = 250000,
                    n.burnin = 25000,
                    n.thin = 10,
                    DIC = F)

### extract log-likelihood and calculate waic score
logRS_0.paramlist <- fit.logRS_0$BUGSoutput$sims.list
logRS_0.loglik <- logRS_0.paramlist$loglik
logRS_0.waic <- waic(logRS_0.loglik)