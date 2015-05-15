############################
# LectureNotes.Day2.r

# Just looks at some of the plots and data that Isabella was working with during
# her presentation
# Day two: Non-parameteric methods

# Created May 13, 2015
# A Putt
############################

# Mann Kendall Non-parametric test
Year <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
Concentration <- c(56, 78, 63, 43, 45, 36, 38, 40, 46, 42)

plot(Concentration ~ Year)
# Add a lowess curve just to get a general idea of the trend.
lines(lowess(Concentration ~ Year))

# There are a ton of different packages
# Even more are shown in the notes
require(Kendall)
MKtest <- Kendall(x=Year, y=Concentration)
summary(MKtest)

require(rkt)
MKtest <- rkt(Year, Concentration)
MKtest

require(EnvStats)
MKtest <- kendallTrendTest(Concentration ~ Year) 
MKtest
names(MKtest)
slope <- MKtest$estimate["slope"]
intercept <- MKtest$estimate["intercept"] # This is nice because it allows you to plot the line
plot(Concentration ~ Year,     
     ylab=expression(paste("Concentration (", mu,"g","/l)")))
abline(a=intercept, b=slope, col="red")
# This line may be misleading. Are we willing to assume that this is a linear trend?
# There actually looks like there is an upward trend at the end.
# This doesn't mean that there is no trend. I just couldn't find one. 
lines(lowess(Concentration~Year),col="blue")
abline(lm(Concentration~Year),col="green")
# You can see that the linear trend is very different from the mann kendall
# But we don't care because the data violoated the linear assumptions...this just 
# illustrates how violations can change your results.

# What if we had censored data, e.g., <0.1
Concentration <- c("5","<2","3","4")

# An interesting way to replace values using stringr
require(stringr)
test <- str_replace_all(Concentration,"<","")

Time <- c(2005,2006,2007,2008)
# We want to test to see if there is a monotonic trend in these data.
require(NADA)

# We need to flag the censored data
Concentration.Cen <- c(FALSE,TRUE,FALSE,FALSE)
Concentration.Val <- c(5,2,3,4) # Create a vector without the censoring

# Cenxyplot sensors the data properly, even though you don't have NAs
cenxyplot(x=Time,xcen=0,y=Concentration.Val,ycen=Concentration.Cen)

MKtest <- cenken(y=Concentration.Val,ycen=Concentration.Cen,x=Time)
MKtest

# This is powerful, but it cannot account for serial correlation


##########
# Seasonal Kendall
require(wq)
chl27 <- sfbayChla[,"s27"]
chl27
plot(chl27)

# Perform the seasonal kendall
seaKen(chl27)
seasonTrend(chl27,plot=FALSE) # Shows the trends by month
seasonTrend(chl27,plot=TRUE,ncol=1,scales="free_y",legend=TRUE)

# The funcion trendHomog(chl27) can also be used to test homogeneity of seasonal trends
trendHomog(chl27)
# The pvalue is large, so we fail to reject the null that the trends are homogenous

# You can also look at this using the rkt package
require(rkt)
Month  <- rep(1:12, length(1978:2008))
Month  <- c(Month,1:8)
Year       <- rep(1978:2008,each=12)
Year       <- c(Year, rep(2009, 8))
# This p-value is looking at the significance of the trend not the homogeneity
# Need to be careful, look at the significance of the trend and the homogeniety of the 
# trends. All of the tests are looking at slighlty different tests/presentations
# There are lots of options so you need to pick the one that works for you

rkt(Year, as.numeric(chl27), Month)

# Or you can use EnvStats
require(EnvStats)
Month  <- rep(1:12, length(1978:2008))
Month  <- c(Month,1:8)
Year       <- rep(1978:2008,each=12)
Year       <- c(Year, rep(2009, 8))

kendallSeasonalTrendTest(as.numeric(chl27) ~ Month + Year)
# p values are very small so we reject the null that the seasonal trends are equal

# Another test in the EnvStats package
SKtest <- kendallSeasonalTrendTest(as.numeric(chl27) ~      
                                     Month + Year)

# The test above also has an option for independent.obs=TRUE. If you set this to 
# false and you get a different result, it could indicate autocorrelation

str(SKtest)

SKtest$estimate
intercept <- SKtest$estimate["intercept"]
slope <- SKtest$estimate["slope"]

# Can also be done in the trend package
# Seasonal Kenall trend package
# load co2 data
data(co2)

# load the trend package
require(trend)

# perform the seasonal kendall test
SK.test <- smk.test(co2)
SK.test

# You can perform a correlated Seasonal Kendall test
# this is very useful if you have correlation between measurements.
SK.test <- csmk.test(co2)
# To assess for autocorrelation you could look at the Acf for individual
# seasons and look for patterns. If you see patterns you may want to use this correlation test.

# You can also perform for multiple stations
chl <- sfbayChla[,1:12]
chl
seaKen(mts2ts(chl,seas=2:4))

##########
# Quantile regression
# Quantile regression is implemented in R via the rq() 
# function available in the quantreg package.

require(quantreg)

#The syntax required for using the rq() function to 
#fit a quantile regression model in R is illustrated 
#below.
#rq(Y ~ X, tau=.5)     # median regression 
#rq(Y ~ X, tau=.1)     # .1 quantile regression
#rq(Y ~ X, tau=.9)     # .9 quantile regression 
#rq(Y ~ X, tau=c(.1,.5,.9))  # all of the above



