library("astsa")

rm(list = ls())
plot(EQ5,col="blue",main="Earthquake & Explosion",ylab="EQ5 & EXP6")
lines(EXP6,col="red",lty=3)

setwd("C:\\Users\\Kun\\Desktop\\485")
heads=scan("coindata.txt")
w=(heads-5)/sqrt(2.5)
x=filter(w,filter=c(0,-0.9),method="recursive")
v=filter(w,rep(1/4,4),sides=1,method="convolution")
plot.ts(x,ylab="x,v",main="x & v of Coin data")
lines(v,col="red",lty="dashed")

W=rnorm(150,0,1)### 50 extra to avoid startup problems
x=filter(W,filter=c(0,-0.9),method="recursive")[-(1:50)]
v=filter(x,rep(1/4,4),sides=1,method="convolution")
plot.ts(x,ylab="AR & MA",main="Autoregression & Moving Average")
lines(v,col="red",lty="dashed")

x=cos(2*pi*1:100/4)
v=filter(x,rep(1/4,4),sides=1,method="convolution")
plot.ts(x,ylab="x,v",main=expression(cos(2*pi/4)~"& Moving Average"))
lines(v,col="red",lty="dashed")

w=rnorm(100,0,1)
v=filter(x+w,rep(1/4,4),sides=1,method="convolution")
plot.ts(x+w,ylab="x+w,v",main=expression(cos(2*pi/4)+N(0,1)~"& Moving Average"))
lines(v,col="red",lty="dashed")


acf=ARMAacf(ma=c(2,1), lag.max=3)
acf
lags=0:3
plot(lags,acf,xlab="Lag",ylab="ACF",type="h", main = "ACF plot")
abline (h=0,col="red")

