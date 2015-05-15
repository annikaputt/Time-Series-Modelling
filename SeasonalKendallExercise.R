############################
# SeasonalKendallExercise.r

# Seasonal Kendall exercise from Time Series Modelling course

# Created May 13, 2015
# A Putt
############################

require(rkt)
data(pie1)
?pie1
str(pie1)

# Move variables to the global environment
Year <- pie1$Year
Month <- pie1$Month
mm <- pie1$mm
SO4 <- pie1$SO4
NO3 <- pie1$NO3 

#Define a Date variable, which will be useful for plotting purposes:
  
Date <- as.Date(paste("01", Month, Year), 
                format="%d %m %Y")
Date 

# Plot the data with the commands below:
par(mfrow=c(3,1), mar=c(5,4,2,2))
plot(mm ~ Date, type="l")
plot(SO4 ~ Date, type="l")
plot(NO3 ~ Date, type="l")

# Are there any decreasing trends visible in the three plots? It is very hard to tell

#Plot the SO4 time series separately for each season (i.e., month).  Do you see 
#consistent decreasing (or increasing) trends over time in SO4 across all seasons?
# Once the data are plotted by season it does look like there is a decreasing seasonal trend

SO4.ts <- ts(SO4, start=c(1998, 1), end=c(2010, 12), frequency=12)

par(mfrow=c(3,4), mar=c(5,4,2,2))
require(forecast)
plot(subset(SO4.ts,month="Jan"), xlab="Year", ylab="SO4", main="Jan")
plot(subset(SO4.ts,month="Feb"), xlab="Year", ylab="SO4", main="Feb")
plot(subset(SO4.ts,month="Mar"), xlab="Year", ylab="SO4", main="Mar")
plot(subset(SO4.ts,month="Apr"), xlab="Year", ylab="SO4", main="Apr")
plot(subset(SO4.ts,month="May"), xlab="Year", ylab="SO4", main="May")
plot(subset(SO4.ts,month="Jun"), xlab="Year", ylab="SO4", main="Jun")
plot(subset(SO4.ts,month="Jul"), xlab="Year", ylab="SO4", main="Jul")
plot(subset(SO4.ts,month="Aug"), xlab="Year", ylab="SO4", main="Aug")
plot(subset(SO4.ts,month="Sep"), xlab="Year", ylab="SO4", main="Sep")
plot(subset(SO4.ts,month="Oct"), xlab="Year", ylab="SO4", main="Oct")
plot(subset(SO4.ts,month="Nov"), xlab="Year", ylab="SO4", main="Nov")
plot(subset(SO4.ts,month="Dec"), xlab="Year", ylab="SO4", main="Dec")

# Let's start with the wq package
require(wq)
seaKen(SO4.ts)
# p value is very small, suggesting an overall seasonal trend

# We can look at the significance of individual trends
seasonTrend(SO4.ts)
seasonTrend(SO4.ts, plot=TRUE, ncol=1, 
            scales="free-y", legend=TRUE)
# Not all trends are significant

# Now we should test if the trends are homogenous
trendHomog(SO4.ts)
# The p value is very large, so we would conclude that although there is an overall
# seasonal trend, the trend is not homogenous accross all seasons

##########
# We can also look at it using the rkt package, which allows us to have 
# an autocorrelation correction
require(rkt)
rkt(Year, SO4, Month, correct=FALSE)
rkt(Year, SO4, Month, correct=TRUE)

# The last command performs a Seasonal Kendall test without 
# correction for correlation among data from different seasons.  
# The p-values between the two tests are very different, so we would
# conclude that there is serial autocorrelation in our data

##########
# We can also look at the envstats package
require(EnvStats)
SKtest <- kendallSeasonalTrendTest(SO4 ~ Month + Year)
SKtest
SKtest$estimate
intercept  <- SKtest$estimate["intercept"]
slope <- SKtest$estimate["slope"]
# this test also has independent.obs, which we can use to look for autocorrelation
SKtest2 <- kendallSeasonalTrendTest(SO4 ~ Month + Year,independent.obs=FALSE)
SKtest2
# Once again, the p-values are very different, suggesting serial autocorrelation

# What if we look at the NO3 data?
NO3
NO3.ts <- ts(NO3,start=c(1998, 1), end=c(2010, 12), frequency=12)

seaKen(NO3.ts)
# p value is small, suggesting an overall seasonal trend

# We can look at the significance of individual trends
seasonTrend(NO3.ts)
seasonTrend(NO3.ts, plot=TRUE, ncol=1, 
            scales="free-y", legend=TRUE)
# Very few trends are significant

trendHomog(NO3.ts)
# The p value is very large, so we would conclude that although there is an overall
# seasonal trend, the trend is not homogenous accross all seasons


