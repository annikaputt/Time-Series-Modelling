############################
# ARIMAexercise.r
# Project: Time-Series-Modelling.rproj

# File description: ARIMA modelling exercise

# Created: May 14, 2015
# R Version: 
# GitHub: yes
# Author: A Putt
############################

install.packages("TSA")
require(TSA)

# obtain the tempdub data
data(tempdub) 
plot(tempdub, ylab="Temperature")
# There is a clear seasonal pattern to these data

# Fit an ARIMA model 
require(forecast)
fit <- auto.arima(tempdub)
summary(fit)

# forecast 2 years ahead 
fcast <- forecast(fit) # The forecast is defaulting to 2 years
plot(fcast) 

# forecast 2 months ahead
fcast <- forecast(fit, h=2)
plot(fcast) 

# Let's model the residuals to see how well we did 
res <- residuals(fit)
res

par(mfrow=c(2,2))
plot(res, xlab="Month", ylab="Residuals")
abline(h=0)
hist(res, main="Histogram of Residuals")
Acf(res, main="Residuals")
Pacf(res, main="Residuals")

# Perform a ljung-box test for autocorrelation in the data
Box.test(res, lag=10, fitdf=0, type="Lj") # If our p value
# is high we fail to reject the hyp that values are independent and there is no 
# serial autocorrelation.

# Produce in-sample accuracy measures of the forecasts:
accuracy(fit)

# Compute one-step ahead forecasts on the test data from some training data:
# See Rob Hyndman’s blog post: http://robjhyndman.com/hyndsight/out-of-sample-one-step-forecasts/  
trainingdata <-  window(tempdub, 1964 + 0/12, 1965 + 11/12)
testdata <- window(tempdub, 1966 + 0/12, 1975 + 11/12)

fit1 <- ets(trainingdata) 
fit2 <- ets(testdata, model=fit1) 
onestep <- fitted(fit2)
onestep 

# Plot the one-step ahead forecasts against the actual values of the test data:
par(mfrow=c(1,1))
plot(as.numeric(onestep) ~ as.numeric(testdata), 
     ylab="One-Step Forecasts", 
     xlab="Actual Test Data Values")





