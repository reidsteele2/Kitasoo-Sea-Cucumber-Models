# Kitasoo-Sea-Cucumber-Models  
Bayesian surplus-production models used to model giant red sea cucumber populations in Kitasoo/Xai'xais experimental fishing areas.  
  
Stan Models:  
ha2W-yearly-hat-hn.stan - Hajas model  
ha2W-yearlyDFO-hat-hn.stan - Hajas model, truncated to 2008 to match DFO modelling  
sc2W-yearly-hat-hn.stan - Schaefer model  
fx2W-yearly-hat-hn.stan - Fox model  
pt2W-yearly-hat-hn.stan - Pella-Tomlinson model  

Simulated Data:
T.simulated_data.RData - Simulated dataset based off of Tolmie Channel
AY_Sim_Modelling&Analyses.R - R code to run RStan modelling and analyses on the simulated dataset

Outdated files:
ha2W-yearly-hat-h.stan - Hajas model  
ha2W-yearlyDFO-hat-h.stan - Hajas model, truncated to 2008 to match DFO modelling  
sc2W-yearly-hat-h.stan - Schaefer model  
fx2W-yearly-hat-h.stan - Fox model  
pt2W-yearly-hat-h.stan - Pella-Tomlinson model  
