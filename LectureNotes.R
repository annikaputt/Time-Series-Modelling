############################
# PresentationFollowAlong.r

# Just looks at some of the plots and data that Isabella was working with during
# her presentation

# Created May 12, 2015
# A Putt
############################# 

##########
# Autocorrelation
require(forecast)
par(mfrow=c(1,2))
# acf spikes represent the correlation between lagged pairs of data
# Eg. linear relationship between pairs of days (day1,day2),(day2,day3),etc
Acf(co2)
# Pacf is more complicated. It is the linear relaionship between residuals from
# residuals from acf linear models agains the original data.
# e.g., lag1 resids vs data, lag2 resids vs data. Take the residuals from both of 
# these models and do a final linear regression.
# The spike is the model correlation.
# lag.plot() # This shows the linear correlations between many different lags

# example of series that is not autocorrelated
x <- rnorm(100)
Acf(x)
Pacf(x)
Pacf(co2)

# If you have high spikes close to one for the whole plot,
# It's an instant givaway of seasonality.
# You must remove the seasonality before you can asses serial correlation.

##########
# Linear Modelling
URL <- url("http://www.cru.uea.ac.uk/cru/data/temperature/HadCRUT3v-gl.dat")
gtemp <- read.table(URL,fill=TRUE)
## Don't need the even rows
gtemp <- gtemp[-seq(2, nrow(gtemp), by = 2), ] 
## set the Year as rownames 
rownames(gtemp) <- gtemp[,1] 
## Add colnames 
colnames(gtemp) <- c("Year", month.abb, "Annual") 
## Work only with 1850-2010 data series 
gtemp <- gtemp[-nrow(gtemp), ] 

# Plot the data
ylab <- expression(Temperature~Anomaly~ (1961-1990)~degree*C) 
par(mfrow=c(1,1))
plot(Annual ~ Year,  data = gtemp, 
     type = "o", ylab = ylab, 
     main = "Global mean temperature anomaly 1850-2010") 

## time series plot of the annual mean temperature
## anomaly for period 1995 - 2010
grecent <- subset(gtemp, 
                  subset = (Year >= 1995 & Year <= 2010), 
                  select = c(Year, Annual)) # Selects only these columns
plot(Annual ~ Year, data = grecent, 
     type = "o", ylab = ylab, 
     main = "Global mean temperature anomaly 1995-2010") 
# Plot suggests clumping (serial correlation) and maybe an outlier

# Fit a linear trend model
m1 <- lm(Annual~Year,data=grecent)
# what is the slope: The change in temperature associated with a 1 unit change in year
# Want the median of the residuals to be roughly zero
# High standard errors indicate a lot of noise
# We would expect sd of the y values to be much higher than the residual
# standard error if the model were significant. In this case it's not, but the model isn't very sig.
# Our overall model isn't very significant, but we have significant slope and intercept
# But biologically, the model isn't very usefull for predicting

# Plot the data with the model
plot(Annual ~ Year, data = grecent, 
     type = "o", ylab = ylab, 
     main = "Global mean temperature anomaly 1995-2010") 
abline(m1, col="blue")

# Does the linear model fit the data better than no trend model
m0 <- lm(Annual~1,data=grecent)
anova(m0,m1) # put the simpler model first

## Predict global mean temperature anomaly for the year 2011 
predict(m1, newdata=data.frame(Year=2011),interval="prediction") 


##########
# Gls
## fit the trend and no trend models using GLS, 
## with maximum likelihood (ML) as the estimation method
## (but no error correlation) 
# This is basically the same as the lm except you are using maximum
# liklihood rather than least squares

require(nlme)
g0 <- gls(Annual ~ 1, data=grecent, method="ML")
g1 <- gls(Annual ~ Year, data=grecent, method="ML")
anova(g0, g1)

g1 <- gls(Annual ~ Year, data=grecent, method="ML")
g1 <- update(g1, method="REML") # We have just changed the estimation to REML
Acf(residuals(g1))
Pacf(residuals(g1))
g2 <- gls(Annual ~ Year, data=grecent, 
          correlation=corARMA(form = ~ Year, p=1), # ar(1) correlation struture)
          method="REML")
Acf(residuals(g2))
g3 <- gls(Annual ~ Year, data=grecent, 
          correlation=corARMA(form = ~ Year, p=2), 
          method="REML")
Acf(residuals(g3))

# Use REML to find the appropriate correlation structure and then use ML
# when trying to figure out how many predictors to include.

anova(g1, g2, g3)
# Doesn't look like autocorrelation is justified in this case.

##########
# Linear models with seasonality but no serial correlation

head(co2)
class(co2)
# This is a time series object
# How would we recreate this? <- ts(co2var,frequency=12,start=c(1959,1),end=c(1997,12))

# Prep the data for later
carbon <- data.frame(CO2 = as.numeric(co2),
                     Month = rep(month.abb, 39),
                     Year = rep(1959:1997, each = 12),
                     Time = seq_len(length(co2)))
carbon <- within(carbon,
                 Date <- as.Date(paste("01", Month, Year),format = "%d %b %Y"))

# Model the data
m1 <- lm(CO2~Time,data=carbon)
summary(m1)
# yes, very significant, but what happens when we look at model diagnostics?

Month_Order <- c("Jan", "Feb", "Mar", 
                 "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", 
                 "Oct", "Nov", "Dec") 
carbon$Month <- factor(carbon$Month, levels=Month_Order)
# or carbon$Month <- factor(carbon$Month,levels=month.abb)

boxplot(resid(m1) ~ Month, data = carbon, las=1, ylab="Residuals")

# Let's add month to our model to account for seasonality
# Month must be a factor
m2 <- lm(CO2 ~ Time+Month,data=carbon)
summary(m2)

boxplot(resid(m2) ~ Month, data = carbon, las=1, ylab="Residuals",
        main="Linear Model with Trend and Season")

#par(mfrow=c(3,1))
res <- ts(resid(m2), s=1959, f=12)
plot.ts(res, ylab="Residuals") 
abline(h=0)
Acf(res, main= "Residuals")
Pacf(res, main= "Residuals")
# Obviously we still have problems with this model. Looks like serial autocorrelation at lag of 1

##########
# Gls linear trend with seasonality and a serial correlation
m2.gls <- gls(CO2 ~ Time + Month, 
              data = carbon, 
              correlation = corAR1(form = ~ Time))
summary(m2.gls)
anova(m2.gls)

##########
# Lm with seasonality and serial correlation with sine/cosine

SIN <- COS <- matrix(nr=length(carbon$Time), nc=6)

for (i in 1:6) {
  COS[,i]<-cos(2*pi*i*carbon$Time/12)
  SIN[,i]<-sin(2*pi*i*carbon$Time/12)
}

m2.har <-lm(CO2 ~ Time +
              COS[,1] + SIN[,1] + 
              COS[,2] + SIN[,2] + 
              COS[,3] + SIN[,3] + 
              COS[,4] + SIN[,4] + 
              COS[,5] + SIN[,5] + 
              COS[,6] + SIN[,6], 
            data=carbon)

summary(m2.har)

#Harmonic coefficients are known to be independent, which means that all harmonic
#coefficients that are not statistically significant can be dropped from the model. 
#The t-ratio can be used to decide which coefficients can be dropped from the 
#model.  It can be obtained by dividing the estimated coefficient by the standard error of the estimate. 
#Coefficients with a t-ratio whose absolute value is less than 2 can be dropped, 
#as they are not statistically significant (where a t-ratio of 2 corresponds to an approximate 5% significance level).
coef(m2.har)/sqrt(diag(vcov(m2.har)))

# we can use stepwise regression to pull the significant coefficients
step(m2.har)

m3.har <-lm(CO2 ~ Time +
              COS[,1] + SIN[,1] + 
              COS[,2], 
            data=carbon)

summary(m3.har)

# Plot the two different models
plot(CO2 ~ Date, data = carbon, type = "l")
lines(fitted(m2) ~ Date, 
      data=carbon, col="blue", lty=2)
lines(fitted(m3.har) ~ Date, 
      data=carbon, col="magenta", lty=3)
legend("topleft", 
       c("Seasonality captured via seasonal dummy variables", 
         "Seasonality captured via harmonic regression function"), 
       lty=c(2,3), col=c("blue", "magenta"))

##########
# Gamm with trend, seasonality, and serial correlation
carbon <- within(carbon,month <- as.numeric(format(Date, format = "%m")))

#Fit a model which includes a smoother for the trend plus a smoother for the seasonal component.
#Furthermore, fit the model assuming AR(1) correlations in the model residuals.

require(mgcv)

m2.gamm <- gamm(CO2 ~ s(Time, bs = "cr") +   
                  s(month, bs = "cc"),
                data = carbon,
                correlation = corAR1(form = ~ Time))

summary(m2.gamm$gam)

summary(m2.gamm$lme)

# Plot the estimated smooth functions of time and month
plot(m2.gamm$gam, pages = 1, scale = 0)

# Plot the gamm
par(mfrow=c(1,1))
plot(CO2 ~ Date, data = carbon, type = "l")
lines(fitted(m2.gamm$lme) ~ Date, data = carbon, col = "red")

# We can formally test if the AR(1) structure is 
#required by fitting a model without the structure 
#and comparing the two models with a likelihood 
#ratio test.

m2.gamm <- gamm(CO2 ~ s(Time, bs = "cr") +   
                  s(month, bs = "cc"),
                data = carbon,
                correlation = corAR1(form = ~ Time))

m3.gamm <- gamm(CO2 ~ s(Time, bs = "cr") + 
                  s(month, bs = "cc"),
                data = carbon)
anova(m2.gamm$lme, m3.gamm$lme) # We have to pull the $lme 

# It looks like m2 fits the data better because the aic is higher



