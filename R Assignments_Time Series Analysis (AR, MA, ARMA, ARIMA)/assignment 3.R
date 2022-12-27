rm(list = ls())
AR1=arima.sim(list(order=c(1,0,0),ar=0.7),n=100)
plot(AR1,ylim=c(-5,5),col="red",ylab="x",main=(expression(AR(1)~~~phi==+0.7)))
acfAR1.var=acf(AR1,type="covariance")
acfAR1.var
acf(AR1,type="correlation")


MA1=arima.sim(list(order=c(0,0,1), ma=0.4), n=100)
plot(MA1,ylim=c(-5,5),col="blue",ylab="x",main=(expression(MA(1)~~~theta==+0.4)))
acfMA1.var=acf(MA1,type="covariance")
acfMA1.var
acf(MA1,type="correlation")

ARMA1=arima.sim(list(order=c(1,0,1),ar=0.7, ma=0.4), n=100)
plot(ARMA1,ylim=c(-5,5),col="green",ylab="x",
     main=(expression(ARMA(1)~~~phi==+0.7~~~theta==+0.4)))



