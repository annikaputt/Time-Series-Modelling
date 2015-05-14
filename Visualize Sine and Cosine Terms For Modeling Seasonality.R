## Sine and Cosine Visualization

t <- 1:24 

par(mfrow=c(6,2), mar=c(2,4,2,2))
j = 1   # values repeat every 12/j = 12 months
plot(cos(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
plot(sin(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
j=2     # values repeat every 12/j = 6 months
plot(cos(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
plot(sin(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
j=3     # values repeat every 12/j = 4 months
plot(cos(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
plot(sin(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
j=4     # values repeat every 12/j = 3 month
plot(cos(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
plot(sin(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
j=5     # values repeat every 12/j = 2.4 months
plot(cos(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)
plot(sin(2*pi*j*t/12),type="b")
abline(v=12.5,col="red",lty=2)


a0 <- 1 
a1 <- 1
b1 <- 1
e <- rnorm(n=24, mean=0, sd=0.1)

Y <- a0 + a1*cos(2*pi*1*t/12) + b1*sin(2*pi*1*t/12) + e
plot(Y, type="b")
abline(v=12.5,col="red",lty=2)


a0 <- 1 
a1 <- 1
b1 <- 1
a2 <- 1
b2 <- 1
e <- rnorm(n=24, mean=0, sd=0.1)

Y <- a0 + a1*cos(2*pi*1*t/12) + b1*sin(2*pi*1*t/12) + 
          a2*cos(2*pi*2*t/12) + b2*sin(2*pi*2*t/12) + e
plot(Y, type="b") 
abline(v=12.5,col="red",lty=2)
abline(v=6.5,col="blue",lty=2)
abline(v=18.5,col="blue",lty=2)



a0 <- 1 
a1 <- 1
b1 <- 1
a2 <- 1
b2 <- 1
a3 <- 1
b3 <- 1
c3 <- 1
e <- rnorm(n=24, mean=0, sd=0.1)

Y <- a0 + a1*cos(2*pi*1*t/12) + b1*sin(2*pi*1*t/12) + 
          a2*cos(2*pi*2*t/12) + b2*sin(2*pi*2*t/12) + 
          a3*cos(2*pi*3*t/12) + b3*sin(2*pi*3*t/12) + e
plot(Y, type="b") 
abline(v=12.5,col="red",lty=2)
abline(v=6.5,col="blue",lty=2)
abline(v=18.5,col="blue",lty=2)
abline(v=4.5,col="green",lty=2)
abline(v=8.5,col="green",lty=2)



