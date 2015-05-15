############################
# MannKendallExercise.r

# Non-seasonal mann kendall exercise

# Created May 13, 2015
# A Putt
############################# 

require(Kendall) 
data(PrecipGL)
?PrecipGL
PrecipGL
plot(PrecipGL)

Year <- 1900:1986
Precip <- as.numeric(PrecipGL)
MKtest <- Kendall(x=Year, y = Precip)
summary(MKtest)

# We haven't tested the assumptions of normality here, but we are just going to use 
# these data to play around with non-parameteric methods

# Our p-value is significant, which is suggesting a trend

# Try another package
require(EnvStats)
MKtest <- kendallTrendTest(Precip ~ Year)
MKtest

# Plot the model fit
slope <- MKtest$estimate["slope"]
intercept <- MKtest$estimate["intercept"]

par(mfrow=c(1,1))
plot(Precip ~ Year, type="l")
abline(a = intercept, b = slope, col="red")
# plot the lowess line too
lines(lowess(Precip~Year), col="blue")
# Looks like the linear trend is capturing the movement of the data

# What if we had missing data?
# Some of the packages can handle missing data, while others cannot
Precip <- c(56,NA,63,43,45,36,38,40,NA,42)
Year <- 1990:1999
MKtest <- kendallTrendTest(Precip ~ Year)
MKtest

# Plot the model fit
slope <- MKtest$estimate["slope"]
intercept <- MKtest$estimate["intercept"]

par(mfrow=c(1,1))
plot(Precip ~ Year, type="p")
abline(a = intercept, b = slope, col="red")

