rm(list = ls())
AR1=arima.sim(model=list(ar=0.6),n=100)
acf(AR1,lag.max=20)
pacf(AR1,lag.max=20)

MA1=arima.sim(model=list(ma=0.9),n=100)
acf(MA1,lag.max=20)
pacf(MA1,lag.max=20)

ARMA1=arima.sim(model=list(ar=0.6,ma=0.9),n=100)
acf(ARMA1,lag.max=20)
pacf(ARMA1,lag.max=20)

acf=ARMAacf(model=list(ar=0.6,ma=0.9), lag.max=3)
acf


library("astsa")
acf2(cmort, 48)
(regr = ar.ols(cmort, order=2, demean=FALSE, intercept=TRUE))
regr$asy.se.coef
regr$var.pred

(fore = predict(regr, n.ahead=4))
ts.plot(cmort, fore$pred, xlim=c(1979,1980), ylab="cardiovascular mortality")
lines(fore$pred, type="p", col=2)
lines(fore$pred+1.96*fore$se, lty="dashed", col=4)
lines(fore$pred-1.96*fore$se, lty="dashed", col=4)
fore$pred+1.96*fore$se
fore$pred-1.96*fore$se

(regr2=ar.yw(cmort,order=2, demean=TRUE, intercept=TRUE))
regr2$asy.se.coef
regr2$var.pred
