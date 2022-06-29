# Model runs using simulated data and subsequent analyses for ***
# Simulated data is based off of Tolmie Channel
# Reid Steele, June 27, 2022

# Libraries
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(rethinking)
library(dplyr)
library(loo)

# Load in fake data
load('T_simulated_data.RData')

##############################################################################################################
##############################################################################################################
##############################################################################################################

# Model runs

##############################################################################################################

# Schaefer

MSC.T.Tr.Sc.inference <- stan(
  file = "sc2W-yearly-hat-hn.stan",  # Stan code
  data = simsave, # named list of data
  init = 0,
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  control = list(max_treedepth = 15, adapt_delta = 0.8), refresh = 100)

precis(MSC.T.Tr.Sc.sim.test)
pairs(MSC.T.Tr.Sc.sim.test, pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT'))
plot(MSC.T.Tr.Sc.sim.test, plotfun = 'trace', pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT'), inc_warmup = TRUE)


# Hajas

MSC.T.Tr.Ha.inference <- stan(
  file = "ha2W-yearly-hat-hn.stan",  # Stan code
  data = simsave, # named list of data
  init = 0,
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  control = list(max_treedepth = 15, adapt_delta = 0.8), refresh = 100)

precis(MSC.T.Tr.Ha.sim.test)
pairs(MSC.T.Tr.Ha.sim.test, pars = c('G', 'LPmax', 'xmax', 'b', 'a', 'sigS', 'sigY', 'sigT'))
plot(MSC.T.Tr.Ha.sim.test, plotfun = 'trace', pars = c('G', 'LPmax', 'xmax', 'b', 'a', 'sigS', 'sigY', 'sigT'), inc_warmup = TRUE)


# Fox

MSC.T.Tr.Fox.inference <- stan(
  file = "fx2W-yearly-hat-hn.stan",  # Stan code
  data = simsave, # named list of data
  init = 0,
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  control = list(max_treedepth = 15, adapt_delta = 0.8), refresh = 100)

precis(MSC.T.Tr.Fox.sim.test)
pairs(MSC.T.Tr.Fox.sim.test, pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT'))
plot(MSC.T.Tr.Fox.sim.test, plotfun = 'trace', pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT'), inc_warmup = TRUE)


# Pella-Tomlinson

MSC.T.Tr.PT.inference <- stan(
  file = "pt2W-yearly-hat-hn.stan",  # Stan code
  data = simsave, # named list of data
  init = 0,
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  control = list(max_treedepth = 15, adapt_delta = 0.8), refresh = 100)

precis(MSC.T.Tr.PT.sim.test)
pairs(MSC.T.Tr.PT.sim.test, pars = c('G', 'LPmax', 'n', 'sigS', 'sigY', 'sigT'))
plot(MSC.T.Tr.PT.sim.test, plotfun = 'trace', pars = c('G', 'LPmax', 'n', 'sigS', 'sigY', 'sigT'), inc_warmup = TRUE)

##############################################################################################################
##############################################################################################################
##############################################################################################################

# Analyses

##############################################################################################################


# Summary stats and plots

# Tolmie Channel

# Schaefer
precis(MSC.T.Tr.Sc.inference)
plot(MSC.T.Tr.Sc.inference, plotfun = 'trace', pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT')) +  
  ggtitle('Tolmie Channel, Schaefer Model') + theme(plot.title = element_text(hjust = 0.5))
pairs(MSC.T.Tr.Sc.inference, pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT'), 
      main = 'Tolmie Channel, Schaefer Model')

# Hajas
precis(MSC.T.Tr.Ha.inference)
plot(MSC.T.Tr.Ha.inference, plotfun = 'trace', pars = c('G', 'LPmax', 'xmax', 'b', 'a', 'sigS', 'sigY','sigT')) + 
  ggtitle('Tolmie Channel, Hajas Model') + theme(plot.title = element_text(hjust = 0.5))
pairs(MSC.T.Tr.Ha.inference, pars = c('G', 'LPmax', 'xmax', 'b', 'a', 'sigS', 'sigY', 'sigT'), 
      main = 'Tolmie Channel, Hajas Model')

# Fox
precis(MSC.T.Tr.Fox.inference)
plot(MSC.T.Tr.Fox.inference, plotfun = 'trace', pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT')) +  
  ggtitle('Tolmie Channel, Fox Model') + theme(plot.title = element_text(hjust = 0.5))
pairs(MSC.T.Tr.Fox.inference, pars = c('G', 'LPmax', 'sigS', 'sigY', 'sigT'), 
      main = 'Tolmie Channel, Fox Model')

# Pella-Tomlinson
precis(MSC.T.Tr.PT.inference)
plot(MSC.T.Tr.PT.inference, plotfun = 'trace', pars = c('G', 'LPmax', 'n', 'sigS', 'sigY', 'sigT')) + 
  ggtitle('Tolmie Channel, Pella-Tomlinson Model') + theme(plot.title = element_text(hjust = 0.5))
pairs(MSC.T.Tr.PT.inference, pars = c('G', 'LPmax', 'n', 'sigS', 'sigY', 'sigT'), 
      main = 'Tolmie Channel, Pella-Tomlinson Model')


# Calculate LOO and WAIC
detach("package:rethinking", unload=TRUE)

# Tolmie Channel

# Schaefer
MSC.T.Tr.Sc.likelihood = extract_log_lik(MSC.T.Tr.Sc.inference, merge_chains = FALSE)
MSC.T.Tr.Sc.r_eff = relative_eff(exp(MSC.T.Tr.Sc.likelihood))
MSC.T.Tr.Sc.LOO = loo(MSC.T.Tr.Sc.likelihood, r_eff = MSC.T.Tr.Sc.r_eff)
print(MSC.T.Tr.Sc.LOO)
MSC.T.Tr.Sc.WAIC = waic(MSC.T.Tr.Sc.likelihood)
print(MSC.T.Tr.Sc.WAIC)
summary(MSC.T.Tr.Sc.inference, pars = 'dev')

# Hajas
MSC.T.Tr.Ha.likelihood = extract_log_lik(MSC.T.Tr.Ha.inference, merge_chains = FALSE)
MSC.T.Tr.Ha.r_eff = relative_eff(exp(MSC.T.Tr.Ha.likelihood))
MSC.T.Tr.Ha.LOO = loo(MSC.T.Tr.Ha.likelihood, r_eff = MSC.T.Tr.Ha.r_eff)
print(MSC.T.Tr.Ha.LOO)
MSC.T.Tr.Ha.WAIC = waic(MSC.T.Tr.Ha.likelihood)
print(MSC.T.Tr.Ha.WAIC)
summary(MSC.T.Tr.Ha.inference, pars = 'dev')

# Fox
MSC.T.Tr.Fox.likelihood = extract_log_lik(MSC.T.Tr.Fox.inference, merge_chains = FALSE)
MSC.T.Tr.Fox.r_eff = relative_eff(exp(MSC.T.Tr.Fox.likelihood))
MSC.T.Tr.Fox.LOO = loo(MSC.T.Tr.Fox.likelihood, r_eff = MSC.T.Tr.Fox.r_eff)
print(MSC.T.Tr.Fox.LOO)
MSC.T.Tr.Fox.WAIC = waic(MSC.T.Tr.Fox.likelihood)
print(MSC.T.Tr.Fox.WAIC)
summary(MSC.T.Tr.Fox.inference, pars = 'dev')

# Pella-Tomlinson
MSC.T.Tr.PT.likelihood = extract_log_lik(MSC.T.Tr.PT.inference, merge_chains = FALSE)
MSC.T.Tr.PT.r_eff = relative_eff(exp(MSC.T.Tr.PT.likelihood))
MSC.T.Tr.PT.LOO = loo(MSC.T.Tr.PT.likelihood, r_eff = MSC.T.Tr.PT.r_eff)
print(MSC.T.Tr.PT.LOO)
MSC.T.Tr.PT.WAIC = waic(MSC.T.Tr.PT.likelihood)
print(MSC.T.Tr.PT.WAIC)
summary(MSC.T.Tr.PT.inference, pars = 'dev')

# Compare
Tolmie.LOO = as.data.frame(compare(MSC.T.Tr.Sc.LOO, MSC.T.Tr.Ha.LOO, MSC.T.Tr.Fox.LOO, MSC.T.Tr.PT.LOO))
Tolmie.WAIC = as.data.frame(compare(MSC.T.Tr.Sc.WAIC, MSC.T.Tr.Ha.WAIC, MSC.T.Tr.Fox.WAIC, MSC.T.Tr.PT.WAIC))
Tolmie.DIC = rbind(summary(MSC.T.Tr.Ha.inference, pars = 'dev')$summary, summary(MSC.T.Tr.PT.inference, pars = 'dev')$summary, summary(MSC.T.Tr.Fox.inference, pars = 'dev')$summary, summary(MSC.T.Tr.Sc.inference, pars = 'dev')$summary)
Tolmie.DIC = as.data.frame(Tolmie.DIC)
rownames(Tolmie.DIC) = c('MSC.T.Tr.Ha.DIC', 'MSC.T.Tr.PT.DIC', 'MSC.T.Tr.Fox.DIC', 'MSC.T.Tr.Sc.DIC')
## plot comparisons
### LOO
plot(1:4 ~ Tolmie.LOO$looic, yaxt = 'n', xlim = c(5500, 5950), pch = 19, ylab = 'Model', xlab = 'LOO Information Criteria', main = 'Tolmie Channel LOOIC')
axis(2, at = 1:4, labels = gsub('.LOO', '', gsub('MSC.T.Tr.', '', rownames(Tolmie.LOO))))
arrows(Tolmie.LOO$looic-Tolmie.LOO$se_looic, 1:4, Tolmie.LOO$looic+Tolmie.LOO$se_looic, 1:4, length=0.05, angle=90, code=3)
### WAIC
plot(1:4 ~ Tolmie.WAIC$waic, yaxt = 'n', xlim = c(5500, 5950), pch = 19, ylab = 'Model', xlab = 'WAIC', main = 'Tolmie Channel WAIC')
axis(2, at = 1:4, labels = gsub('.WAIC', '', gsub('MSC.T.Tr.', '', rownames(Tolmie.WAIC))))
arrows(Tolmie.WAIC$waic-Tolmie.WAIC$se_waic, 1:4, Tolmie.WAIC$waic+Tolmie.WAIC$se_waic, 1:4, length=0.05, angle=90, code=3)
### DIC
plot(1:4 ~ Tolmie.DIC$mean, yaxt = 'n', xlim = c(5400, 5445), pch = 19, ylab = 'Model', xlab = 'DIC', main = 'Tolmie Channel DIC')
axis(2, at = 1:4, labels = gsub('.DIC', '', gsub('MSC.T.Tr.', '', rownames(Tolmie.DIC))))
arrows(Tolmie.DIC$mean-Tolmie.DIC$sd, 1:4, Tolmie.DIC$mean+Tolmie.DIC$sd, 1:4, length=0.05, angle=90, code=3)


# Annual harvest simulation 
library(rethinking)

# timesim and rotsim functions

## Hajas and Schaefer
timesimSH = function(Et, LPmax, xmax = 0.5, b = 1, dt = 1/12, xi = 0.999, hor_F = 75, hor_NF = 25){ # Write function to sim 75 years of fishing
  output = xi
  for(i in 2:((hor_F/dt)+1)){
    if(output[i-1] >= 1){output = c(output, 1)} else {
      if(output[i-1] <= 0){output = c(output, 0)} else {
        x <- output[i-1]
        a <-(b*(xmax/(1-xmax)))
        LP <- LPmax*((x/xmax)^a)*(((1-x)/(1-xmax))^b)
        dLP <- (LP)*(LP-Et)*((a/x)-(b/(1-x)))
        xy <- x + dt*(LP-Et) + ((dt^2)/2)*(dLP)
        if(is.nan(xy)){xy = 0}
        output = c(output, xy)}}
  }
  for(i in ((hor_F/dt)+2):((((hor_F/dt)+1))+(hor_NF/dt))){
    if(output[i-1] >= 1){output = c(output, 1)} else {
      if(output[i-1] <= 0){output = c(output, 0)} else {
        x <- output[i-1]
        a <-(b*(xmax/(1-xmax)))
        LP <- LPmax*((x/xmax)^a)*(((1-x)/(1-xmax))^b)
        dLP <- (LP)*(LP-Et)*((a/x)-(b/(1-x)))
        xy <- x + dt*(LP-0) + ((dt^2)/2)*(dLP)
        if(is.nan(xy)){xy = 1}
        output = c(output, xy)}}
  }
  output = ifelse(output > 1, 1, output)
  output = ifelse(output < 0, 0, output)
  return(output)}

## Fox
timesimFox = function(Et, LPmax, dt = 1/12, xi = 0.999, hor_F = 75, hor_NF = 25){ # Write function to sim 75 years of fishing
  output = xi
  for(i in 2:((hor_F/dt)+1)){
    if(output[i-1] >= 1){output = c(output, 1)} else {
      if(output[i-1] <= 0){output = c(output, 0)} else {
        x <- output[i-1]
        LP <- -exp(1)*LPmax*x*log(x)
        dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
        xy <- x + dt*(LP-Et) + ((dt^2)/2)*(dLP)
        output = c(output, xy)}}
  }
  for(i in ((hor_F/dt)+2):((((hor_F/dt)+1))+(hor_NF/dt))){
    if(output[i-1] >= 1){output = c(output, 1)} else {
      if(output[i-1] <= 0){output = c(output, 0)} else {
        x <- output[i-1]
        LP <- -exp(1)*LPmax*x*log(x)
        dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
        xy <- x + dt*(LP-0) + ((dt^2)/2)*(dLP)
        output = c(output, xy)}}
  }
  output = ifelse(output > 1, 1, output)
  output = ifelse(output < 0, 0, output)
  return(output)}

## Pella-Tomlinson
timesimPT = function(Et, LPmax, n, dt = 1/12, xi = 0.999, hor_F = 75, hor_NF = 25){ # Write function to sim 75 years of fishing
  output = xi
  for(i in 2:((hor_F/dt)+1)){
    if(output[i-1] >= 1){output = c(output, 1)} else {
      if(output[i-1] <= 0){output = c(output, 0)} else {
        x <- output[i-1]
        gamma <- (n^(n/(n-1)))/(n-1)
        LP <- (gamma*LPmax*x)-(gamma*LPmax*(x^n))
        dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
        xy <- x + dt*(LP-Et) + ((dt^2)/2)*(dLP)
        output = c(output, xy)}}
  }
  for(i in ((hor_F/dt)+2):((((hor_F/dt)+1))+(hor_NF/dt))){
    if(output[i-1] >= 1){output = c(output, 1)} else {
      if(output[i-1] <= 0){output = c(output, 0)} else {
        x <- output[i-1]
        gamma <- (n^(n/(n-1)))/(n-1)
        LP <- (gamma*LPmax*x)-(gamma*LPmax*(x^n))
        dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
        xy <- x + dt*(LP-0) + ((dt^2)/2)*(dLP)
        output = c(output, xy)}}
  }
  output = ifelse(output > 1, 1, output)
  output = ifelse(output < 0, 0, output)
  return(output)}

# Rotational simulation, 75 years fishing 25 years no fishing
## Full/Schaefer
rotsimSH = function(Et, LPmax, xmax = 0.5, b = 1, dt = 1/12, xi = 0.999, hor_F = 75, hor_NF = 25){ # Write function to sim 75 years of fishing
  output = xi
  for(i in 1:ceiling(hor_F/3)){
    for(j in 1:(1/dt)){
      x <- output[((i-1)*((1/dt)*3))+j]
      if(x == 0){xy = 0} else {
        if(x >= 1){x = 0.999}
        a <-(b*(xmax/(1-xmax)))
        LP <- LPmax*((x/xmax)^a)*(((1-x)/(1-xmax))^b)
        dLP <- (LP)*(LP-Et)*((a/x)-(b/(1-x)))
        xy <- x + dt*(LP-Et) + ((dt^2)/2)*(dLP)}
      if(is.nan(xy)){xy = 0}
      if(xy <= 0){xy = 0}
      output = c(output, xy)}
    for(j in 1:((1/dt)*2)){
      x <- output[(((i-1)*((1/dt)*3))+(1/dt))+j]
      if(x == 0){xy = 0} else {
        if(x == 1){xy = 1} else{
          a <-(b*(xmax/(1-xmax)))
          LP <- LPmax*((x/xmax)^a)*(((1-x)/(1-xmax))^b)
          dLP <- (LP)*(LP)*((a/x)-(b/(1-x)))
          xy <- x + dt*(LP) + ((dt^2)/2)*(dLP)}}
      if(is.nan(xy)){xy = 1}
      if(xy >= 1){xy = 1}
      output = c(output, xy)}
  }
  for(i in ((ceiling(hor_F/3)*(3/(dt)))+1):(((hor_F/dt))+(hor_NF/dt))){
    x <- output[i]
    if(x >= 1){xy = 1} else {if(x <= 0){xy == 0} else {
      a <-(b*(xmax/(1-xmax)))
      LP <- LPmax*((x/xmax)^a)*(((1-x)/(1-xmax))^b)
      dLP <- (LP)*(LP-Et)*((a/x)-(b/(1-x)))
      xy <- x + dt*(LP-0) + ((dt^2)/2)*(dLP)}}
    output = c(output, xy)
  }
  output = ifelse(output >= 1, 1, output)
  return(output)}

## Fox
rotsimFox = function(Et, LPmax, dt = 1/12, xi = 0.999, hor_F = 75, hor_NF = 25){ # Write function to sim 75 years of fishing
  output = xi
  for(i in 1:ceiling(hor_F/3)){
    for(j in 1:(1/dt)){
      x <- output[((i-1)*((1/dt)*3))+j]
      if(x == 0){xy = 0} else {
        if(x >= 1){x = 0.999}
        LP <- -exp(1)*LPmax*x*log(x)
        dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
        xy <- x + dt*(LP-Et) + ((dt^2)/2)*(dLP)}
      if(is.nan(xy)){xy = 0}
      if(xy <= 0){xy = 0}
      output = c(output, xy)}
    for(j in 1:((1/dt)*2)){
      x <- output[(((i-1)*((1/dt)*3))+(1/dt))+j]
      if(x == 0){xy = 0} else {
        if(x == 1){xy = 1} else{
          LP <- -exp(1)*LPmax*x*log(x)
          dLP <- (LP)*(LP)*((1/x)-(1/(1-x)))
          xy <- x + dt*(LP) + ((dt^2)/2)*(dLP)}}
      if(is.nan(xy)){xy = 1}
      if(xy >= 1){xy = 1}
      output = c(output, xy)}
  }
  for(i in ((ceiling(hor_F/3)*(3/(dt)))+1):(((hor_F/dt))+(hor_NF/dt))){
    x <- output[i]
    if(x >= 1){xy = 1} else {if(x <= 0){xy == 0} else {
      LP <- -exp(1)*LPmax*x*log(x)
      dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
      xy <- x + dt*(LP-0) + ((dt^2)/2)*(dLP)}}
    output = c(output, xy)
  }
  return(output)}

## Pella-Tomlinson
rotsimPT = function(Et, LPmax, n, dt = 1/12, xi = 0.999, hor_F = 75, hor_NF = 25){ # Write function to sim 75 years of fishing
  output = xi
  for(i in 1:ceiling(hor_F/3)){
    for(j in 1:(1/dt)){
      x <- output[((i-1)*((1/dt)*3))+j]
      if(x == 0){xy = 0} else {
        if(x >= 1){x = 0.999}
        gamma <- (n^(n/(n-1)))/(n-1)
        LP <- (gamma*LPmax*x)-(gamma*LPmax*(x^n))
        dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
        xy <- x + dt*(LP-Et) + ((dt^2)/2)*(dLP)}
      if(is.nan(xy)){xy = 0}
      if(xy <= 0){xy = 0}
      output = c(output, xy)}
    for(j in 1:((1/dt)*2)){
      x <- output[(((i-1)*((1/dt)*3))+(1/dt))+j]
      if(x == 0){xy = 0} else {
        if(x == 1){xy = 1} else{
          gamma <- (n^(n/(n-1)))/(n-1)
          LP <- (gamma*LPmax*x)-(gamma*LPmax*(x^n))
          dLP <- (LP)*(LP)*((1/x)-(1/(1-x)))
          xy <- x + dt*(LP) + ((dt^2)/2)*(dLP)}}
      if(is.nan(xy)){xy = 1}
      if(xy >= 1){xy = 1}
      output = c(output, xy)}
  }
  for(i in ((ceiling(hor_F/3)*(3/(dt)))+1):(((hor_F/dt))+(hor_NF/dt))){
    x <- output[i]
    if(x >= 1){xy = 1} else {if(x <= 0){xy == 0} else {
      gamma <- (n^(n/(n-1)))/(n-1)
      LP <- (gamma*LPmax*x)-(gamma*LPmax*(x^n))
      dLP <- (LP)*(LP-Et)*((1/x)-(1/(1-x)))
      xy <- x + dt*(LP-0) + ((dt^2)/2)*(dLP)}}
    output = c(output, xy)
  }
  return(output)}

# Plot Time Series Simulations
time = seq(from = 0, to = 200, by = 1)
index = sample(1:4000, 100)
crt = 'probability of crashing is '

# Tolmie Channel

# Setup

# Hajas
T.Ha.summ = summary(MSC.T.Tr.Ha.inference, pars = c('LPmax', 'b', 'xmax'))
T.Ha.summ = as.data.frame(T.Ha.summ$summary)
T.Ha.m.xmax = T.Ha.summ$`50%`[3]; T.Ha.m.b = T.Ha.summ$`50%`[2]; T.Ha.m.LPmax = T.Ha.summ$`50%`[1]
T.Ha.samples = rstan::extract(MSC.T.Tr.Ha.inference)
T.Ha.uncertainties = as.data.frame(cbind(T.Ha.samples$xmax[index], T.Ha.samples$b[index], T.Ha.samples$LPmax[index]))
colnames(T.Ha.uncertainties) = c('xmax', 'b', 'LPmax')

# Schaefer
T.Sc.summ = summary(MSC.T.Tr.Sc.inference, pars = 'LPmax')
T.Sc.summ = as.data.frame(T.Sc.summ$summary)
T.Sc.m.LPmax = T.Sc.summ$`50%`[1]
T.Sc.samples = rstan::extract(MSC.T.Tr.Sc.inference)
T.Sc.uncertainties = as.data.frame(cbind(T.Sc.samples$LPmax[index]))
colnames(T.Sc.uncertainties) = 'LPmax'

# Fox
T.Fox.summ = summary(MSC.T.Tr.Fox.inference, pars = 'LPmax')
T.Fox.summ = as.data.frame(T.Fox.summ$summary)
T.Fox.m.LPmax = T.Fox.summ$`50%`[1]
T.Fox.samples = rstan::extract(MSC.T.Tr.Fox.inference)
T.Fox.uncertainties = as.data.frame(cbind(T.Fox.samples$LPmax[index]))
colnames(T.Fox.uncertainties) = 'LPmax'

# Pella-Tomlinson
T.PT.summ = summary(MSC.T.Tr.PT.inference, pars = c('LPmax', 'n'))
T.PT.summ = as.data.frame(T.PT.summ$summary)
T.PT.m.LPmax = T.PT.summ$`50%`[1]; T.PT.m.n = T.PT.summ$`50%`[2]
T.PT.samples = rstan::extract(MSC.T.Tr.PT.inference)
T.PT.uncertainties = as.data.frame(cbind(T.PT.samples$LPmax[index], T.PT.samples$n[index]))
colnames(T.PT.uncertainties) = c('LPmax', 'n')

# 14% Harvest

# 14% annual harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.a = timesimSH(0.14, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.14, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.a = cbind(MSC.T.Tr.Ha.uncertainties.a, comb)
}
MSC.T.Tr.Ha.uncertainties.a = as.data.frame(MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.ts.a = data.frame(time, MSC.T.Tr.Ha.curve.a, MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.a[,3] ~ time, data = MSC.T.Tr.Ha.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 14% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.a), side = 3)

# 14% rotational harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.r = rotsimSH(0.42, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.42, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.r = cbind(MSC.T.Tr.Ha.uncertainties.r, comb)
}
MSC.T.Tr.Ha.uncertainties.r = as.data.frame(MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.ts.r = data.frame(time, MSC.T.Tr.Ha.curve.r, MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.r[,3] ~ time, data = MSC.T.Tr.Ha.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 14% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.r), side = 3)


# 14% annual harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.a = timesimSH(0.14, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.14, T.Sc.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Sc.uncertainties.a = cbind(MSC.T.Tr.Sc.uncertainties.a, comb)
}
MSC.T.Tr.Sc.uncertainties.a = as.data.frame(MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.ts.a = data.frame(time, MSC.T.Tr.Sc.curve.a, MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.a[,3] ~ time, data = MSC.T.Tr.Sc.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 14% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.a), side = 3)

# 14% rotational harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.r = rotsimSH(0.42, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.42, T.Sc.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Sc.uncertainties.r = cbind(MSC.T.Tr.Sc.uncertainties.r, comb)
}
MSC.T.Tr.Sc.uncertainties.r = as.data.frame(MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.ts.r = data.frame(time, MSC.T.Tr.Sc.curve.r, MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.r[,3] ~ time, data = MSC.T.Tr.Sc.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 14% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.r), side = 3)


# 14% annual harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.a = timesimFox(0.14, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimFox(0.14, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.a = cbind(MSC.T.Tr.Fox.uncertainties.a, comb)
}
MSC.T.Tr.Fox.uncertainties.a = as.data.frame(MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.ts.a = data.frame(time, MSC.T.Tr.Fox.curve.a, MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.a[,3] ~ time, data = MSC.T.Tr.Fox.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 14% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.a), side = 3)

# 14% rotational harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.r = rotsimFox(0.42, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimFox(0.42, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.r = cbind(MSC.T.Tr.Fox.uncertainties.r, comb)
}
MSC.T.Tr.Fox.uncertainties.r = as.data.frame(MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.ts.r = data.frame(time, MSC.T.Tr.Fox.curve.r, MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.r[,3] ~ time, data = MSC.T.Tr.Fox.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 14% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.r), side = 3)


# 14% annual harvest, PT Model

## Data
MSC.T.Tr.PT.curve.a = timesimPT(0.14, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimPT(0.14, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.a = cbind(MSC.T.Tr.PT.uncertainties.a, comb)
}
MSC.T.Tr.PT.uncertainties.a = as.data.frame(MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.ts.a = data.frame(time, MSC.T.Tr.PT.curve.a, MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.a[,3] ~ time, data = MSC.T.Tr.PT.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 14% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.a), side = 3)

# 14% rotational harvest, PT Model

## Data
MSC.T.Tr.PT.curve.r = rotsimPT(0.42, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimPT(0.42, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.r = cbind(MSC.T.Tr.PT.uncertainties.r, comb)
}
MSC.T.Tr.PT.uncertainties.r = as.data.frame(MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.ts.r = data.frame(time, MSC.T.Tr.PT.curve.r, MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.r[,3] ~ time, data = MSC.T.Tr.PT.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 14% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.r), side = 3)


# 6.7% Harvest

# 6.7% annual harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.a = timesimSH(0.067, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.067, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.a = cbind(MSC.T.Tr.Ha.uncertainties.a, comb)
}
MSC.T.Tr.Ha.uncertainties.a = as.data.frame(MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.ts.a = data.frame(time, MSC.T.Tr.Ha.curve.a, MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.a[,3] ~ time, data = MSC.T.Tr.Ha.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 6.7% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.a), side = 3)

# 6.7% rotational harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.r = rotsimSH(0.201, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.201, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.r = cbind(MSC.T.Tr.Ha.uncertainties.r, comb)
}
MSC.T.Tr.Ha.uncertainties.r = as.data.frame(MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.ts.r = data.frame(time, MSC.T.Tr.Ha.curve.r, MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.r[,3] ~ time, data = MSC.T.Tr.Ha.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 6.7% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.r), side = 3)


# 6.7% annual harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.a = timesimSH(0.067, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.067, T.Sc.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Sc.uncertainties.a = cbind(MSC.T.Tr.Sc.uncertainties.a, comb)
}
MSC.T.Tr.Sc.uncertainties.a = as.data.frame(MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.ts.a = data.frame(time, MSC.T.Tr.Sc.curve.a, MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.a[,3] ~ time, data = MSC.T.Tr.Sc.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 6.7% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.a), side = 3)

# 6.7% rotational harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.r = rotsimSH(0.201, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.201, T.Sc.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Sc.uncertainties.r = cbind(MSC.T.Tr.Sc.uncertainties.r, comb)
}
MSC.T.Tr.Sc.uncertainties.r = as.data.frame(MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.ts.r = data.frame(time, MSC.T.Tr.Sc.curve.r, MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.r[,3] ~ time, data = MSC.T.Tr.Sc.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 6.7% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.r), side = 3)


# 6.7% annual harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.a = timesimFox(0.067, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimFox(0.067, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.a = cbind(MSC.T.Tr.Fox.uncertainties.a, comb)
}
MSC.T.Tr.Fox.uncertainties.a = as.data.frame(MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.ts.a = data.frame(time, MSC.T.Tr.Fox.curve.a, MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.a[,3] ~ time, data = MSC.T.Tr.Fox.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 6.7% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.a), side = 3)

# 6.7% rotational harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.r = rotsimFox(0.201, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimFox(0.201, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.r = cbind(MSC.T.Tr.Fox.uncertainties.r, comb)
}
MSC.T.Tr.Fox.uncertainties.r = as.data.frame(MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.ts.r = data.frame(time, MSC.T.Tr.Fox.curve.r, MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.r[,3] ~ time, data = MSC.T.Tr.Fox.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 6.7% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.r), side = 3)


# 6.7% annual harvest, PT Model

## Data
MSC.T.Tr.PT.curve.a = timesimPT(0.067, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimPT(0.067, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.a = cbind(MSC.T.Tr.PT.uncertainties.a, comb)
}
MSC.T.Tr.PT.uncertainties.a = as.data.frame(MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.ts.a = data.frame(time, MSC.T.Tr.PT.curve.a, MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.a[,3] ~ time, data = MSC.T.Tr.PT.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 6.7% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.a), side = 3)

# 6.7% rotational harvest, PT Model

## Data
MSC.T.Tr.PT.curve.r = rotsimPT(0.201, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimPT(0.201, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.r = cbind(MSC.T.Tr.PT.uncertainties.r, comb)
}
MSC.T.Tr.PT.uncertainties.r = as.data.frame(MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.ts.r = data.frame(time, MSC.T.Tr.PT.curve.r, MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.r[,3] ~ time, data = MSC.T.Tr.PT.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 6.7% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.r), side = 3)


# 4.2% Harvest

# 4.2% annual harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.a = timesimSH(0.042, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.042, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.a = cbind(MSC.T.Tr.Ha.uncertainties.a, comb)
}
MSC.T.Tr.Ha.uncertainties.a = as.data.frame(MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.ts.a = data.frame(time, MSC.T.Tr.Ha.curve.a, MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.a[,3] ~ time, data = MSC.T.Tr.Ha.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 4.2% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.a), side = 3)

# 4.2% rotational harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.r = rotsimSH(0.126, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.126, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.r = cbind(MSC.T.Tr.Ha.uncertainties.r, comb)
}
MSC.T.Tr.Ha.uncertainties.r = as.data.frame(MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.ts.r = data.frame(time, MSC.T.Tr.Ha.curve.r, MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.r[,3] ~ time, data = MSC.T.Tr.Ha.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 4.2% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.r), side = 3)


# 4.2% annual harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.a = timesimSH(0.042, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.042, T.Sc.uncertainties$LPmax[i])
  MSC.T.Tr.Sc.uncertainties.a = cbind(MSC.T.Tr.Sc.uncertainties.a, comb, dt = 1, hor_F = 175)
}
MSC.T.Tr.Sc.uncertainties.a = as.data.frame(MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.ts.a = data.frame(time, MSC.T.Tr.Sc.curve.a, MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.a[,3] ~ time, data = MSC.T.Tr.Sc.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 4.2% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.a), side = 3)

# 4.2% rotational harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.r = rotsimSH(0.126, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.126, T.Sc.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Sc.uncertainties.r = cbind(MSC.T.Tr.Sc.uncertainties.r, comb)
}
MSC.T.Tr.Sc.uncertainties.r = as.data.frame(MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.ts.r = data.frame(time, MSC.T.Tr.Sc.curve.r, MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.r[,3] ~ time, data = MSC.T.Tr.Sc.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 4.2% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.r), side = 3)


# 4.2% annual harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.a = timesimFox(0.042, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimFox(0.042, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.a = cbind(MSC.T.Tr.Fox.uncertainties.a, comb)
}
MSC.T.Tr.Fox.uncertainties.a = as.data.frame(MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.ts.a = data.frame(time, MSC.T.Tr.Fox.curve.a, MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.a[,3] ~ time, data = MSC.T.Tr.Fox.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 4.2% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.a), side = 3)

# 4.2% rotational harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.r = rotsimFox(0.126, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimFox(0.126, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.r = cbind(MSC.T.Tr.Fox.uncertainties.r, comb)
}
MSC.T.Tr.Fox.uncertainties.r = as.data.frame(MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.ts.r = data.frame(time, MSC.T.Tr.Fox.curve.r, MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.r[,3] ~ time, data = MSC.T.Tr.Fox.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 4.2% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.r), side = 3)


# 4.2% annual harvest, PT Model

## Data
MSC.T.Tr.PT.curve.a = timesimPT(0.042, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimPT(0.042, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.a = cbind(MSC.T.Tr.PT.uncertainties.a, comb)
}
MSC.T.Tr.PT.uncertainties.a = as.data.frame(MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.ts.a = data.frame(time, MSC.T.Tr.PT.curve.a, MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.a[,3] ~ time, data = MSC.T.Tr.PT.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 4.2% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.a), side = 3)

# 4.2% rotational harvest, PT Model

## Data
MSC.T.Tr.PT.curve.r = rotsimPT(0.126, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimPT(0.126, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.r = cbind(MSC.T.Tr.PT.uncertainties.r, comb)
}
MSC.T.Tr.PT.uncertainties.r = as.data.frame(MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.ts.r = data.frame(time, MSC.T.Tr.PT.curve.r, MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.r[,3] ~ time, data = MSC.T.Tr.PT.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 4.2% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.r), side = 3)



# 3.33% Harvest

# 3.33% annual harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.a = timesimSH(0.0333, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.0333, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.a = cbind(MSC.T.Tr.Ha.uncertainties.a, comb)
}
MSC.T.Tr.Ha.uncertainties.a = as.data.frame(MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.ts.a = data.frame(time, MSC.T.Tr.Ha.curve.a, MSC.T.Tr.Ha.uncertainties.a)
MSC.T.Tr.Ha.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.a[,3] ~ time, data = MSC.T.Tr.Ha.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 3.33% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.a), side = 3)

# 3.33% rotational harvest, Hajas Model

## Data
MSC.T.Tr.Ha.curve.r = rotsimSH(0.1, T.Ha.m.LPmax, T.Ha.m.xmax, T.Ha.m.b, dt = 1, hor_F = 175)
MSC.T.Tr.Ha.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.1, T.Ha.uncertainties$LPmax[i], T.Ha.uncertainties$xmax[i], T.Ha.uncertainties$b[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Ha.uncertainties.r = cbind(MSC.T.Tr.Ha.uncertainties.r, comb)
}
MSC.T.Tr.Ha.uncertainties.r = as.data.frame(MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.ts.r = data.frame(time, MSC.T.Tr.Ha.curve.r, MSC.T.Tr.Ha.uncertainties.r)
MSC.T.Tr.Ha.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Ha.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Ha.ts.r[,3] ~ time, data = MSC.T.Tr.Ha.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Hajas model, 3.33% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Ha.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Ha.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Ha.crash.r), side = 3)


# 3.33% annual harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.a = timesimSH(0.0333, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimSH(0.0333, T.Sc.uncertainties$LPmax[i])
  MSC.T.Tr.Sc.uncertainties.a = cbind(MSC.T.Tr.Sc.uncertainties.a, comb, dt = 1, hor_F = 175)
}
MSC.T.Tr.Sc.uncertainties.a = as.data.frame(MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.ts.a = data.frame(time, MSC.T.Tr.Sc.curve.a, MSC.T.Tr.Sc.uncertainties.a)
MSC.T.Tr.Sc.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.a[,3] ~ time, data = MSC.T.Tr.Sc.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 3.33% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.a), side = 3)

# 3.33% rotational harvest, Schaefer Model

## Data
MSC.T.Tr.Sc.curve.r = rotsimSH(0.1, T.Sc.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Sc.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimSH(0.1, T.Sc.uncertainties$LPmax[i])
  MSC.T.Tr.Sc.uncertainties.r = cbind(MSC.T.Tr.Sc.uncertainties.r, comb, dt = 1, hor_F = 175)
}
MSC.T.Tr.Sc.uncertainties.r = as.data.frame(MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.ts.r = data.frame(time, MSC.T.Tr.Sc.curve.r, MSC.T.Tr.Sc.uncertainties.r)
MSC.T.Tr.Sc.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Sc.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Sc.ts.r[,3] ~ time, data = MSC.T.Tr.Sc.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Schaefer model, 3.33% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Sc.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Sc.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Sc.crash.r), side = 3)


# 3.33% annual harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.a = timesimFox(0.0333, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimFox(0.0333, T.Fox.uncertainties$LPmax[i])
  MSC.T.Tr.Fox.uncertainties.a = cbind(MSC.T.Tr.Fox.uncertainties.a, comb, dt = 1, hor_F = 175)
}
MSC.T.Tr.Fox.uncertainties.a = as.data.frame(MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.ts.a = data.frame(time, MSC.T.Tr.Fox.curve.a, MSC.T.Tr.Fox.uncertainties.a)
MSC.T.Tr.Fox.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.a[,3] ~ time, data = MSC.T.Tr.Fox.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 3.33% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.a), side = 3)

# 3.33% rotational harvest, Fox Model

## Data
MSC.T.Tr.Fox.curve.r = rotsimFox(0.1, T.Fox.m.LPmax, dt = 1, hor_F = 175)
MSC.T.Tr.Fox.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimFox(0.1, T.Fox.uncertainties$LPmax[i], dt = 1, hor_F = 175)
  MSC.T.Tr.Fox.uncertainties.r = cbind(MSC.T.Tr.Fox.uncertainties.r, comb)
}
MSC.T.Tr.Fox.uncertainties.r = as.data.frame(MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.ts.r = data.frame(time, MSC.T.Tr.Fox.curve.r, MSC.T.Tr.Fox.uncertainties.r)
MSC.T.Tr.Fox.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.Fox.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.Fox.ts.r[,3] ~ time, data = MSC.T.Tr.Fox.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Fox model, 3.33% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.Fox.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.Fox.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.Fox.crash.r), side = 3)


# 3.33% annual harvest, PT Model

## Data
MSC.T.Tr.PT.curve.a = timesimPT(0.0333, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.a = NULL
comb = NULL
for(i in 1:100){
  comb = timesimPT(0.0333, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.a = cbind(MSC.T.Tr.PT.uncertainties.a, comb)
}
MSC.T.Tr.PT.uncertainties.a = as.data.frame(MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.ts.a = data.frame(time, MSC.T.Tr.PT.curve.a, MSC.T.Tr.PT.uncertainties.a)
MSC.T.Tr.PT.crash.a = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.a[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.a[,3] ~ time, data = MSC.T.Tr.PT.ts.a, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 3.33% annual harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.a[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.a ~ time, col = 'red', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.a), side = 3)

# 3.33% rotational harvest, PT Model

## Data
MSC.T.Tr.PT.curve.r = rotsimPT(0.1, T.PT.m.LPmax, T.PT.m.n, dt = 1, hor_F = 175)
MSC.T.Tr.PT.uncertainties.r = NULL
comb = NULL
for(i in 1:100){
  comb = rotsimPT(0.1, T.PT.uncertainties$LPmax[i], T.PT.uncertainties$n[i], dt = 1, hor_F = 175)
  MSC.T.Tr.PT.uncertainties.r = cbind(MSC.T.Tr.PT.uncertainties.r, comb)
}
MSC.T.Tr.PT.uncertainties.r = as.data.frame(MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.ts.r = data.frame(time, MSC.T.Tr.PT.curve.r, MSC.T.Tr.PT.uncertainties.r)
MSC.T.Tr.PT.crash.r = round((sum(ifelse(sapply(MSC.T.Tr.PT.ts.r[,-1], min) <= 0.0, 1, 0))/101), digits = 3)

## Plot
plot(MSC.T.Tr.PT.ts.r[,3] ~ time, data = MSC.T.Tr.PT.ts.r, type = 'l', col = alpha(rgb(0,0,0), 0.2), 
     lty = 'solid', main = 'Tolmie Channel Pella-Tomlinson model, 3.33% rotational harvest', 
     xlab = 'Years', ylab = 'Biomass as a fraction of virgin biomass (x)', ylim = c(0,1))
for(i in 4:102){lines(MSC.T.Tr.PT.ts.r[,i] ~ time, col = alpha(rgb(0,0,0), 0.2), lty = 'solid')}
lines(MSC.T.Tr.PT.curve.r ~ time, col = 'blue', lwd = 2, lty = 'solid')
mtext(text = paste0(crt, MSC.T.Tr.PT.crash.r), side = 3)


# LP over X curves
x = seq(from = 0.01, to = 0.99, by = 0.01)
# Tolmie Channel

# Hajas Model
curve((T.Ha.uncertainties$LPmax[1]*((x/T.Ha.uncertainties$xmax[1])^(T.Ha.uncertainties$b[1]*(T.Ha.uncertainties$xmax[1]/(1-T.Ha.uncertainties$xmax[1]))))*((1-x)/(1-T.Ha.uncertainties$xmax[1]))^T.Ha.uncertainties$b[1]), 
      from = 0, to = 1, ylab = 'LP(x)', ylim = c(0, 0.25), main = 'Tolmie Channel, Hajas Model', col  = col.alpha('black', 0.2))
for(i in 2:100){
  T.Ha.l.xmax = T.Ha.uncertainties$xmax[i]
  T.Ha.l.LPmax = T.Ha.uncertainties$LPmax[i]
  T.Ha.l.b = T.Ha.uncertainties$b[i]
  curve((T.Ha.l.LPmax*((x/T.Ha.l.xmax)^(T.Ha.l.b*(T.Ha.l.xmax/(1-T.Ha.l.xmax))))*((1-x)/(1-T.Ha.l.xmax))^T.Ha.l.b),
        add = TRUE, col = col.alpha('black', 0.2))
}
curve((T.Ha.m.LPmax*((x/T.Ha.m.xmax)^(T.Ha.m.b*(T.Ha.m.xmax/(1-T.Ha.m.xmax))))*((1-x)/(1-T.Ha.m.xmax))^T.Ha.m.b), 
      from = 0, to = 1, add = TRUE, col = 'red', lwd = 3)

# Schaefer Model
curve((T.Sc.uncertainties$LPmax[1]*((x/0.5)^(1*(0.5/(1-0.5))))*((1-x)/(1-0.5))^1), 
      from = 0, to = 1, ylab = 'LP(x)', ylim = c(0, 0.25), main = 'Tolmie Channel, Schaefer Model', col  = col.alpha('black', 0.2))
for(i in 2:100){
  T.Sc.l.xmax = T.Sc.uncertainties$xmax[i]
  T.Sc.l.LPmax = T.Sc.uncertainties$LPmax[i]
  T.Sc.l.b = T.Sc.uncertainties$b[i]
  curve((T.Sc.l.LPmax*((x/0.5)^(1*(0.5/(1-0.5))))*((1-x)/(1-0.5))^1),
        add = TRUE, col = col.alpha('black', 0.2))
}
curve((T.Sc.m.LPmax*((x/0.5)^(1*(0.5/(1-0.5))))*((1-x)/(1-0.5))^1), 
      from = 0, to = 1, add = TRUE, col = 'red', lwd = 3)

# Fox Model
curve(-exp(1)*T.Fox.uncertainties$LPmax[1]*x*log(x), 
      from = 0, to = 1, ylab = 'LP(x)', ylim = c(0, 0.25), main = 'Tolmie Channel, Fox Model', col  = col.alpha('black', 0.2))
for(i in 2:100){
  T.Fox.l.xmax = T.Fox.uncertainties$xmax[i]
  T.Fox.l.LPmax = T.Fox.uncertainties$LPmax[i]
  T.Fox.l.b = T.Fox.uncertainties$b[i]
  curve(-exp(1)*T.Fox.l.LPmax*x*log(x),
        add = TRUE, col = col.alpha('black', 0.2))
}
curve((-exp(1)*T.Fox.m.LPmax*x*log(x)), 
      from = 0, to = 1, add = TRUE, col = 'red', lwd = 3)

# Pella-Tomlinson Model
curve((((T.PT.uncertainties$n[1]^(T.PT.uncertainties$n[1]/(T.PT.uncertainties$n[1]-1)))/(T.PT.uncertainties$n[1]-1))*T.PT.uncertainties$LPmax[1]*x)-(((T.PT.uncertainties$n[1]^(T.PT.uncertainties$n[1]/(T.PT.uncertainties$n[1]-1)))/(T.PT.uncertainties$n[1]-1))*T.PT.uncertainties$LPmax[1]*(x^T.PT.uncertainties$n[1])), 
      from = 0, to = 1, ylab = 'LP(x)', ylim = c(0, 0.25), main = 'Tolmie Channel, Pella-Tomlinson Model', col  = col.alpha('black', 0.2))
for(i in 2:100){
  T.PT.l.LPmax = T.PT.uncertainties$LPmax[i]
  T.PT.l.n = T.PT.uncertainties$n[i]
  curve((((T.PT.l.n^(T.PT.l.n/(T.PT.l.n-1)))/(T.PT.l.n-1))*T.PT.l.LPmax*x)-(((T.PT.l.n^(T.PT.l.n/(T.PT.l.n-1)))/(T.PT.l.n-1))*T.PT.l.LPmax*(x^T.PT.l.n)),
        add = TRUE, col = col.alpha('black', 0.2))
}
curve((((T.PT.m.n^(T.PT.m.n/(T.PT.m.n-1)))/(T.PT.m.n-1))*T.PT.m.LPmax*x)-(((T.PT.m.n^(T.PT.m.n/(T.PT.m.n-1)))/(T.PT.m.n-1))*T.PT.m.LPmax*(x^T.PT.m.n)), 
      from = 0, to = 1, add = TRUE, col = 'red', lwd = 3)




# Crash Rate

# crashrate function - returns probability of crash based on model
crashrate = function(model, Et, type = 'annual', modeltype , rep = 4000, mab = 0, delta = 1/12, Fhor = 75, NFhor = 25){
  df = NULL; count = 0
  lines = as.data.frame(rstan::extract(model))
  index = sample(1:4000, rep)
  
  if(type == 'annual' & modeltype == 'Hajas'){
    for(i in 1:rep){
      comb = timesimSH(Et, LPmax = lines$LPmax[index[i]], b = lines$b[index[i]], xmax = lines$xmax[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'annual' & modeltype == 'Schaefer'){
    df = NULL
    for(i in 1:rep){
      comb = timesimSH(Et, LPmax = lines$LPmax[index[i]], b = 1, xmax = 0.5, dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'annual' & modeltype == 'Fox'){
    df = NULL
    for(i in 1:rep){
      comb = timesimFox(Et, LPmax = lines$LPmax[index[i]], hor_F = Fhor, dt = delta, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'annual' & modeltype == 'PT'){
    for(i in 1:rep){
      comb = timesimPT(Et, LPmax = lines$LPmax[index[i]], n = lines$n[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'Hajas'){
    for(i in 1:rep){
      comb = rotsimSH(Et*3, LPmax = lines$LPmax[index[i]], b = lines$b[index[i]], xmax = lines$xmax[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'Schaefer'){
    df = NULL
    for(i in 1:rep){
      comb = rotsimSH(Et*3, LPmax = lines$LPmax[index[i]], b = 1, xmax = 0.5, dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'Fox'){
    df = NULL
    for(i in 1:rep){
      comb = rotsimFox(Et*3, LPmax = lines$LPmax[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'PT'){
    for(i in 1:rep){
      comb = rotsimPT(Et*3, LPmax = lines$LPmax[index[i]], n = lines$n[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  df = as.data.frame(df)
  count = sum(ifelse(sapply(df, min) <= mab, 1, 0))
  
  cr = count/rep
  return(cr)
}


crashratedb = function(model, Et, type = 'annual', modeltype , rep = 4000, mab = 0, delta = 1/12, Fhor = 75, NFhor = 25){
  df = NULL; count = 0
  lines = as.data.frame(rstan::extract(model))
  index = 1:4000
  
  if(type == 'annual' & modeltype == 'Hajas'){
    for(i in 1:rep){
      comb = timesimSH(Et, LPmax = lines$LPmax[index[i]], b = lines$b[index[i]], xmax = lines$xmax[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'annual' & modeltype == 'Schaefer'){
    df = NULL
    for(i in 1:rep){
      comb = timesimSH(Et, LPmax = lines$LPmax[index[i]], b = 1, xmax = 0.5, dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'annual' & modeltype == 'Fox'){
    df = NULL
    for(i in 1:rep){
      comb = timesimFox(Et, LPmax = lines$LPmax[index[i]], hor_F = Fhor, dt = delta, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'annual' & modeltype == 'PT'){
    for(i in 1:rep){
      comb = timesimPT(Et, LPmax = lines$LPmax[index[i]], n = lines$n[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'Hajas'){
    for(i in 1:rep){
      comb = rotsimSH(Et*3, LPmax = lines$LPmax[index[i]], b = lines$b[index[i]], xmax = lines$xmax[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'Schaefer'){
    df = NULL
    for(i in 1:rep){
      comb = rotsimSH(Et*3, LPmax = lines$LPmax[index[i]], b = 1, xmax = 0.5, dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'Fox'){
    df = NULL
    for(i in 1:rep){
      comb = rotsimFox(Et*3, LPmax = lines$LPmax[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  if(type == 'rotational' & modeltype == 'PT'){
    for(i in 1:rep){
      comb = rotsimPT(Et*3, LPmax = lines$LPmax[index[i]], n = lines$n[index[i]], dt = delta, hor_F = Fhor, hor_NF = NFhor)
      df = cbind(df, comb)}}
  
  df = as.data.frame(df)
  count = sum(ifelse(sapply(df, min) <= mab, 1, 0))
  
  cr = count/rep
  return(df)
}


# crvec function - Vectorize Crash Rates over a range of Et values in order to plot
crvec = function(model, Etmax, type, modeltype, Etmin = 0.01, interval = 0.01, MAB = 0, deltat = 1/12, F.hor = 75, NF.hor = 25){
  accumulate = NULL
  for(i in seq(Etmin, Etmax, interval)){
    add = crashrate(model, i, type, modeltype, mab = MAB, delta = deltat, Fhor = F.hor, NFhor = NF.hor)
    accumulate = c(accumulate, add)
    print(i)}
  return(accumulate)
}

# Generate Crash Rate Data
Exploitation.T = seq(from = 0.005, to = 0.15, by = 0.005)

# Tolmie Channel

## Data
T.Hajas.Annual = crvec(MSC.T.Tr.Ha.inference, 0.15, 'annual', 'Hajas', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.Hajas.Rotational = crvec(MSC.T.Tr.Ha.inference, 0.15, 'rotational', 'Hajas', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.Schaefer.Annual = crvec(MSC.T.Tr.Sc.inference, 0.15, 'annual', 'Schaefer', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.Schaefer.Rotational = crvec(MSC.T.Tr.Sc.inference, 0.15, 'rotational', 'Schaefer', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.Fox.Annual = crvec(MSC.T.Tr.Fox.inference, 0.15, 'annual', 'Fox', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.Fox.Rotational = crvec(MSC.T.Tr.Fox.inference, 0.15, 'rotational', 'Fox', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.PT.Annual = crvec(MSC.T.Tr.PT.inference, 0.15, 'annual', 'PT', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.PT.Rotational = crvec(MSC.T.Tr.PT.inference, 0.15, 'rotational', 'PT', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005)
T.CrashRateData = data.frame(Exploitation.T, T.Hajas.Annual, T.Hajas.Rotational, T.Schaefer.Annual, T.Schaefer.Rotational, T.Fox.Annual, T.Fox.Rotational, T.PT.Annual, T.PT.Rotational)


## Plot
plot(T.Hajas.Annual ~ Exploitation.T, type = 'b', col = 'red', pch = 15, ylab = 'Probability of population crash', xlab = 'Exploitation rate', ylim = c(0,1), main = 'Tolmie Channel')
lines(T.Hajas.Rotational ~ Exploitation.T, type = 'b', col = 'red', pch = 16)
lines(T.Schaefer.Annual ~ Exploitation.T, type = 'b', col = 'blue', pch = 15)
lines(T.Schaefer.Rotational ~ Exploitation.T, type = 'b', col = 'blue', pch = 16)
lines(T.Fox.Annual ~ Exploitation.T, type = 'b', col = 'green', pch = 15)
lines(T.Fox.Rotational ~ Exploitation.T, type = 'b', col = 'green', pch = 16)
lines(T.PT.Annual ~ Exploitation.T, type = 'b', col = 'purple', pch = 15)
lines(T.PT.Rotational ~ Exploitation.T, type = 'b', col = 'purple', pch = 16)
legend('right', legend = c('Hajas', 'Schaefer', 'Fox', 'Pella-Tomlinson'), col = c('red', 'blue', 'green', 'purple'), bty = 'n', lty = 1)
legend('left', legend = c('Annual', 'Rotational'), col = 'black', bty = 'n', pch = c(15,16))



# 50% biomass not exceeded


Exploitation50.T = seq(from = 0.005, to = 0.10, by = 0.005)


# Tolmie Channel

## Data
T.Hajas.Annual50 = crvec(MSC.T.Tr.Ha.inference, 0.10, 'annual', 'Hajas', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.Hajas.Rotational50 = crvec(MSC.T.Tr.Ha.inference, 0.10, 'rotational', 'Hajas', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.Schaefer.Annual50 = crvec(MSC.T.Tr.Sc.inference, 0.10, 'annual', 'Schaefer', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.Schaefer.Rotational50 = crvec(MSC.T.Tr.Sc.inference, 0.10, 'rotational', 'Schaefer', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.Fox.Annual50 = crvec(MSC.T.Tr.Fox.inference, 0.10, 'annual', 'Fox', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.Fox.Rotational50 = crvec(MSC.T.Tr.Fox.inference, 0.10, 'rotational', 'Fox', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.PT.Annual50 = crvec(MSC.T.Tr.PT.inference, 0.10, 'annual', 'PT', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.PT.Rotational50 = crvec(MSC.T.Tr.PT.inference, 0.10, 'rotational', 'PT', deltat = 1, F.hor = 175, Etmin = 0.005, interval = 0.005, MAB = 0.5)
T.CrashRateData50 = data.frame(Exploitation50, T.Hajas.Annual50, T.Hajas.Rotational50, T.Schaefer.Annual50, T.Schaefer.Rotational50, T.Fox.Annual50, T.Fox.Rotational50, T.PT.Annual50, T.PT.Rotational50)


## Plot
plot(T.Hajas.Annual50 ~ Exploitation50.T, type = 'l', col = 'red', pch = 15, lwd = 2,
     ylab = 'Probability of exceeding 50% of virgin biomass', xlab = 'Exploitation rate', ylim = c(0,1), main = 'Tolmie Channel')
lines(T.Hajas.Rotational50 ~ Exploitation50.T, type = 'l', col = 'red', pch = 16, lty = 2, lwd = 2)
lines(T.Schaefer.Annual50 ~ Exploitation50.T, type = 'l', col = 'blue', pch = 15, lwd = 2)
lines(T.Schaefer.Rotational50 ~ Exploitation50.T, type = 'l', col = 'blue', pch = 16, lty = 2, lwd = 2)
lines(T.Fox.Annual50 ~ Exploitation50.T, type = 'l', col = 'green', pch = 15, lwd = 2)
lines(T.Fox.Rotational50 ~ Exploitation50.T, type = 'l', col = 'green', pch = 16, lty = 2, lwd = 2)
lines(T.PT.Annual50 ~ Exploitation50.T, type = 'l', col = 'purple', pch = 15, lwd = 2)
lines(T.PT.Rotational50 ~ Exploitation50.T, type = 'l', col = 'purple', pch = 16, lty = 2, lwd = 2)
legend('right', legend = c('Hajas', 'Schaefer', 'Fox', 'Pella-Tomlinson'), col = c('red', 'blue', 'green', 'purple'), bty = 'n', lty = 1, lwd = 2)
legend('left', legend = c('Annual', 'Rotational'), col = 'black', bty = 'n', lty = c(1,2))



# 90% biomass not exceeded


Exploitation90.T = seq(from = 0.001, to = 0.05, by = 0.001)


# Tolmie Channel

## Data
T.Hajas.Annual90 = crvec(MSC.T.Tr.Ha.inference, 0.05, 'annual', 'Hajas', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.Hajas.Rotational90 = crvec(MSC.T.Tr.Ha.inference, 0.05, 'rotational', 'Hajas', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.Schaefer.Annual90 = crvec(MSC.T.Tr.Sc.inference, 0.05, 'annual', 'Schaefer', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.Schaefer.Rotational90 = crvec(MSC.T.Tr.Sc.inference, 0.05, 'rotational', 'Schaefer', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.Fox.Annual90 = crvec(MSC.T.Tr.Fox.inference, 0.05, 'annual', 'Fox', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.Fox.Rotational90 = crvec(MSC.T.Tr.Fox.inference, 0.05, 'rotational', 'Fox', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.PT.Annual90 = crvec(MSC.T.Tr.PT.inference, 0.05, 'annual', 'PT', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.PT.Rotational90 = crvec(MSC.T.Tr.PT.inference, 0.05, 'rotational', 'PT', deltat = 1, F.hor = 175, Etmin = 0.001, interval = 0.001, MAB = 0.9)
T.CrashRateData90 = data.frame(Exploitation90.T, T.Hajas.Annual90, T.Hajas.Rotational90, T.Schaefer.Annual90, T.Schaefer.Rotational90, T.Fox.Annual90, T.Fox.Rotational90, T.PT.Annual90, T.PT.Rotational90)


## Plot
plot(T.Hajas.Annual90 ~ Exploitation90.T, type = 'b', col = 'red', pch = 15, ylab = 'Probability of exceeding 90% of virgin biomass', xlab = 'Exploitation rate', ylim = c(0,1), main = 'Tolmie Channel')
lines(T.Hajas.Rotational90 ~ Exploitation90.T, type = 'b', col = 'red', pch = 16)
lines(T.Schaefer.Annual90 ~ Exploitation90.T, type = 'b', col = 'blue', pch = 15)
lines(T.Schaefer.Rotational90 ~ Exploitation90.T, type = 'b', col = 'blue', pch = 16)
lines(T.Fox.Annual90 ~ Exploitation90.T, type = 'b', col = 'green', pch = 15)
lines(T.Fox.Rotational90 ~ Exploitation90.T, type = 'b', col = 'green', pch = 16)
lines(T.PT.Annual90 ~ Exploitation90.T, type = 'b', col = 'purple', pch = 15)
lines(T.PT.Rotational90 ~ Exploitation90.T, type = 'b', col = 'purple', pch = 16)
legend('right', legend = c('Hajas', 'Schaefer', 'Fox', 'Pella-Tomlinson'), col = c('red', 'blue', 'green', 'purple'), bty = 'n', lty = 1)
legend('bottomright', legend = c('Annual', 'Rotational'), col = 'black', bty = 'n', pch = c(15,16))


# Divergences

# Divergence calculation function
divergences = function(model, warmup = 1000){
  
  # Gather diagnostics
  diag = get_sampler_params(model)
  
  # Initialize divergence counter
  div = 0
  
  # Loop through chains
  for(i in 1:length(diag)){
    
    div = div + sum(as.data.frame(diag[[i]])$divergent__[-1:-warmup])
    
  }
  
  # Return number of divergent transitions
  return(div)
  
}

# Calculate divergence number for each model
MSC.T.Tr.Sc.div = divergences(MSC.T.Tr.Sc.inference)
MSC.T.Tr.Ha.div = divergences(MSC.T.Tr.Ha.inference)
MSC.T.Tr.Fox.div = divergences(MSC.T.Tr.Fox.inference)
MSC.T.Tr.PT.div = divergences(MSC.T.Tr.PT.inference)
