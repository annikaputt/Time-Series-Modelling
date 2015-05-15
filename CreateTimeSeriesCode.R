############################
# CreateTimeSeriesCode.r

# Just some code to play around with creating time series'

# Created May 13, 2015
# A Putt
############################


# For example, create quarterly data
value <- rnorm(n=100*4,mean=10)
head(value)
ts.val <- ts(value,frequency=4,start=c(1900,1),end=c(2000,4))
par(mfrow=c(2,2))
plot(subset(ts.val,quarter=1))
plot(subset(ts.val,quarter=2))
plot(subset(ts.val,quarter=3))
plot(subset(ts.val,quarter=4))
