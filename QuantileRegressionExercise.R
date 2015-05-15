############################
# QuantileRegressionExercise.r
# Project: Time-Series-Modelling.Rproj

# File description: Exercise in quantile regression from time series modelling course

# Created: May 14, 2015
# R Version: 3.2.0
# GitHub: Yes
# Author: A Putt
############################

# Read hurricane data into R 
website <-"http://freakonometrics.free.fr/extremedatasince1899.csv"

StormMax <- read.table(website,header=TRUE,sep=",")

# Subset the cyclones by basin (cyclones away from the
# U.S. coastline) and by year after 1977 (satellite era)
StormMaxBasin <- subset(StormMax,Region=="Basin") 
StormMaxBasin <- subset(StormMaxBasin, Yr>1977) 

# Attach cyclone data to current R working space
attach(StormMaxBasin)
head(StormMaxBasin)

# Conduct a median reqression (i.e., quantile regression 
# with tau=0.5) with Wmax as the response variable and 
# Yr, soi and sst as the explanatory variables:
  
require(quantreg)
tau <- 0.5 # This will give us the median regression
m <- rq(Wmax ~ Yr + soi + sst, tau=0.5) # Quantile regression function 
summary(m, se="iid") # presumes errors are iid (Independent and equal distributions). There are a couple options for this
summary.rq(m)

# Do any of the explanatory variables have significant effects on 
# the median Wmax (i.e., median cyclone intensity)?
# No, all of the p values are quite large

# Conduct an ANOVA analysis comparing the
# median regression model which includes Yr, soi 
# and sst against the one which includes Yr only:
  
m0 <-  rq(Wmax ~ Yr, tau=0.5)  
m <- rq(Wmax ~ Yr + soi + sst, tau=0.5)  
anova(m0, m)
# The p-value is quite large, so we favour the simpler model

# Now conduct quantile regressions with:
taus <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90) 
# For these regressions, treat Wmax as the response variable 
# and Yr, soi and sst as the explanatory variables.  

models <- rq(Wmax ~ Yr + soi + sst, tau=taus)
summary(models, se="boot") # another option other than iid

# Do any of the explanatory variables exhibit significant effects on 
# Wmax (cyclone intensity) as the value of tau increases?
# When tau reaches 0.6 the soi becomes a significant predictor
# There are some increases in significance for the 70th percentile as well, but it's not consistent

