## This script generates some data for change point regression

set.seed(17)
beta0 <- 1
beta1 <- 2 
beta2 <- 4
Year <- 1972:2015
Year2000 <- pmax(0, Year - 2000)
n <- length(Year)
error <- rnorm(n, mean=0, sd=5)
Outcome <- beta0 + beta1*Year2000 + error

plot(Outcome ~ Year)
abline(v=2000)
m <- lm(Outcome~Year)
abline(m,col="red")
mres <- residuals(m)
plot(Year,mres,main="linear model residuals")
abline(h=0)
# Obviously we have issues with our residuals in this model.

#=====================================
# Model using change point regression at 2000
changepoint <- pmax(0,Year-2000)
m1 <- lm(Outcome~Year+changepoint)
# The changepoint is highly significant
# Lets plot the new trendline
plot(Outcome ~ Year,main="Lm fit and changepoint fit")
abline(m,col="red")
lines(fitted(m1)~Year,col="blue")

summary(m1)
# We can see that the changepoint is significant
# We can see that based on the year p value, there is no linear trend before the change
# But, in order to know whether there is a significant trend you need to set up a changepoint

require(multcomp)
K <- matrix(c(0,1,1),1)
T <- glht(m1,linfct=K)
summary(T)
# From this summary we can see that the slope after the change is highly significant.

#============================================
# Can we fit this model with a gls that accounts for autocorrelation?
require(nlme)
m2 <- gls(Outcome~Year+changepoint)
# The estimated values are very similar to the lm model. 
# Just the optimization has changed
m3 <- gls(Outcome~Year+changepoint,correlation=corARMA(form=~Year,p=1))
