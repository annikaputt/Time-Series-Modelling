############################
# TrendExercise.r

# Exercise based on Trend, Serial Correlation.pptx

# Created May 12, 2015
# A Putt
############################# 

require(Kendall) 
data(PrecipGL)
?PrecipGL
PrecipGL
plot(PrecipGL)

# PrecipGL is actually a time series object
# <- ts(PrecipGL,frequency=1,start=2000,end=2003)

Year <-  1900:1986 
m <- lm(PrecipGL ~ Year) 
plot(PrecipGL) # Because it's a time series we don't need to specify year
abline(m, col="red")

summary(m) 
coef(m)
confint(m)

newdata <-  data.frame(Year=1987)
predict(m, newdata = newdata, interval="prediction")

require(forecast)
Acf(residuals(m))
Pacf(residuals(m))
# We don't see serial autocorrelation, which suggests that we have removed the trend

# Fit with a gls using a lag of 1
require(nlme)
Data <- data.frame(PrecipGL, Year)
g <- gls(PrecipGL ~ Year, 
         correlation=corARMA(p=1), 
         method="REML", data=Data) 
summary(g)
g2 <- gls(PrecipGL ~ Year, 
         correlation=corARMA(p=2), 
         method="REML", data=Data) 

# Plot the two different models, they are the same
plot(PrecipGL)
abline(m)
abline(g, col="red", lty=2)

# Make an acf of the normalized residuals
require(forecast)
Pacf(residuals(g, type= "normalized"))
Acf(residuals(g, type = "normalized")) 

# Compare the results
coef(m)
coef(g)
confint(m)
confint(g)

# To predict using gls we need a new package
install.packages("AICcmodavg") 
require(AICcmodavg)
predictSE.gls (g, newdata, se.fit=T) 
predict(m,newdata,interval="prediction")
