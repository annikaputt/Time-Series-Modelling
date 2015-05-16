############################
# LectureNotes.Day3.r

# Just looks at some of the plots and data that Isabella was working with during
# her presentation
# Day three: Time series modelling (ARMA and ARIMA models)

# Created May 14, 2015
# A Putt
############################

############################
# AR(p) models
# The errors in these models are given and AR(1) (or some other level of p) structure

set.seed(1789)

# AR 1 model
# When you think you have AR1 data, you should look for three things
# 1. Do I see a stationary trend? (no change overall through time)
# 2. Take a look at the acf plot: Are the acf spikes going down?
# 3. Look at the pacf plot: Area the spikes at the beginning of the pacf significant?

# Simulate data with phi=0.5
ar.sim <- arima.sim(model=list(ar=c(0.5)),n=100)
# The phi value controls the degree of correlation between consecutive points
# If the phi value is quite high it might lead to an artifial trend due to the high correlation
ar.sim

par(mfrow=c(1,1))
ts.plot(ar.sim)

require(forecast)
par(mfrow=c(1,2))
ar.acf <- Acf(ar.sim)
ar.acf

ar.pacf<-Pacf(ar.sim)
ar.pacf

# AR2 Models
# Here there are two phis. The first is the correlation between the point and the previous point
# the second is the correlation between the point and the point two time steps before.
set.seed(124)

# Create a model with phi 0.9 and phi -0.1
ar.sim<-arima.sim(model=list(ar=c(0.9,-0.1)),n=100)
ar.sim

par(mfrow=c(1,1))
ts.plot(ar.sim)

par(mfrow=c(1,2))
ar.acf<-Acf(ar.sim)
ar.acf

ar.pacf<-Pacf(ar.sim)
ar.pacf

#===============================
# MA(q)
# The moving average model regards the current value of the time series as a 
# moving average (unevenly weighted)
# q is used to denote the order of the moving average model
# Here, the correlation you are looking for is in the random noise, rather than
# The points themselves, so we want the PACF plot to decline. The acf will be more random


set.seed(8537)

ma.sim<-arima.sim(model=list(ma=c(0.9)),n=100)
ma.sim

par(mfrow=c(1,1))
ts.plot(ma.sim)

par(mfrow=c(1,2))
ma.acf<-Acf(ma.sim) # This looks more random, like our previous pacf
ma.acf

ma.pacf<-Pacf(ma.sim) # Here we see more of a pattern
ma.pacf

##########
# ARIMA models
# blend of AR and MA models
# If p or q are zero, it goes back to either the AR or MA model
# Much more difficult to really understand what kind of model you have (what are p and q?)

set.seed(3978)

# Lets simulate one with one AR and one MA term
arma.sim<-arima.sim(model=list(ar=c(0.7),ma=c(0.5)),n=100)
arma.sim 

par(mfrow=c(1,1))
ts.plot(arma.sim)

par(mfrow=c(1,2))
arma.acf<-Acf(arma.sim)
arma.acf

arma.pacf<-Pacf(arma.sim)
arma.pacf

# It is hard to interpret what is going on here and what process is dominating
# That's why we would automate the process

##########
# Fitting an ARMA model (stationary data!)
# The process is called the box-jenkins method
# 1. Identification: Determinethe structure (i.e., what are p and q?)
# This is hard, so we will automate it.
# Once we have found these terms we will fix them for the estimation
# 2. Estimation: Estimate the parameters of the ARMA model
# 3. Diagnostics: Determine the adequacy of the final model

# Let's generate some data and analyze with ARMA
set.seed(234)
ringindex <- arima.sim(model=list(ar=c(0.7)), n=100, sd=sqrt(0.001))
ringindex <- ringindex + 0.2

ringindex <- ts(data=ringindex, start=1915, frequency=1)
par(mfrow=c(1,1))
plot.ts(ringindex, xlab="Year", ylab="Ring Index")
# This time series looks fairly stationary, so we want to use an AR, MA, or ARMA model
par(mfrow=c(1,2))
Acf(ringindex)
Acf(ringindex,lag.max=100) # You can see that it is eventually dying off
Pacf(ringindex)
# The acf and pacf plots also suggest that this is an AR, MA, or ARMA model, 
# but it is very hard to determine the p and q

# Lets fit a model assuming AR(1) based on our spike in the pacf at lag of 1
ringindex.model <- arima(ringindex, order=c(1,0,0)) # c(p,d,q)
# When d is 0, it's equivalent to an ARMA model
# D is the differencing parameter, which we don't have to worry about because we have stationary data
summary(ringindex.model)

# Or, we can fit the order automatically using the forecast package
ringindex.best <- auto.arima(ringindex,seasonal=FALSE,allowdrift=FALSE)
# Seasonal is false because we have no seasonality
# allowdrift is false because we have no trend
# In this case, even if we say true we get the same result because it's just ot there
summary(ringindex.best)

# Lets check to see if we cleared up our residual patterns
Acf(residuals(ringindex.best))
Pacf(residuals(ringindex.best))
# There are still some significant spikes, but we have definitely accounted for some of our correlated processes

# Finally, lets use the forecast package to predict values
fcast <- forecast(ringindex.best,h=10)
par(mfrow=c(1,1))
plot(fcast)
# In the title, the 12 shows that you have a 12 step season
# The first 3 arima terms describe the non-seasonal part
# The second 3 arima terms describe the seasonal part
# Each of the three numbers represent p, d, and q

##########
# ARIMA Modelling
# ARIMA models can handle seasonal and trends in data
# Now we have a non-stationary data set

# Lake Huron data set
data(LakeHuron)
?LakeHuron

# Time series plot of Lake Huron’s depth 
Depth <- as.numeric(LakeHuron)
Year <- seq(1875, 1972, by=1)

plot(Depth ~ Year, type="l")
# Clearly we have a trend in the data

par(mfrow=c(1,2))
Acf(Depth)
Pacf(Depth)

# Now let's use an ARIMA model
# We will set p, q, and d
Depth.model <- arima(Depth,order=c(1,1,2)) # We have set the differencing to 1
# to remove the trend, but we haven't added a seasonal componenet. We 
# will do that later when we look at seasonal co2 data
Depth.model

# How do we choose p, d, and q
par(mfrow=c(3,2))
Depth <- ts(Depth, start=1875, frequency=1)
plot(Depth, type="l")

# Plot the first order difference
plot(diff(Depth,1), type="l")

# Check the acf and pacf from the original model and the differenced model
Acf(Depth)
Pacf(Depth)
Acf(diff(Depth,1))
Pacf(diff(Depth,1))
# It looks like one order of differencing was enough to make the data stationary
# Now we can use auto arima to find the best model, given that we already know what d should be due to the trend

Depth.model <- auto.arima(Depth,seasonal=FALSE,allowdrift=FALSE) 
# Drift is not the same as trend, so we still set that as false
Depth.model

##########
# Seasonal ARIMA models
data(co2)
?co2

require(graphics)
plot(co2, ylab = expression("Atmospheric concentration of CO"[2]),las = 1)
title(main = "co2 data set")
par(mfrow=c(1,2))
Acf(co2)
Pacf(co2)

# We clearly need to difference this series for trend
plot(diff(co2,1))
acf(diff(co2,1))
pacf(diff(co2,1))
# Here, the order of the differencing, 1, indicates that we applied a 
# non-seasonal difference  to the co2 series.

# We also need to difference this model for seasonality
plot(diff(diff(co2,1),12))
acf(diff(diff(co2,1),12))
pacf(diff(diff(co2,1),12))
# Here, the order of the differencing, 12, indicates 
# that we applied a seasonal difference  to the differenced co2 series.
# The auto.arima can do the differencing for us, but this way we can visually
# see that we have removed the non-stationarity and seasonality in the time series.

# The seasonal ARIMA model we will fit to the co2 series will have the 
# following components: 
# A nonseasonal component (composed of p, q, and d)
# A seasonal component (composed of p, q, and d)

co2.model <- Arima(co2, order=c(0,1,1), 
                   seasonal=c(0,1,1))
# Note that this is different than the arima function from package(stats)
# Although we can have auto.arima fit this model for us, it may be better
# to use this manual method because we know what is happening here.
# auto.arima could be used to guide us if need be.

# Let's look at the diagnostics and forecast
Acf(residuals(co2.model))
Pacf(residuals(co2.model))
library(portes)
LjungBox(residuals(co2.model), lags=20)
# Our p value is high so we fail to reject the hypothesis that
# the values are independent (no correlation)
fcast <- forecast(co2.model, h=12)
plot(fcast)
fcast

# OR, we can use the automated function to find the best arima model p, q, and d
co2.best <- auto.arima(co2, 
                       seasonal=TRUE, 
                       allowdrift=FALSE)
summary(co2.best) # We get a slighlty different result

# Let's do some diagnostics
Acf(residuals(co2.best))
Pacf(residuals(co2.best))
Box.test(residuals(co2.best), 
         lag=20, type="Ljung") # P value is high, no evidence of correlation



