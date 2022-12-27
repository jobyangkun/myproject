w=rnorm(500,mean=0,sd=1)
w=as.ts(w)
acf.w=acf(w,lag.max=20,type="correlation")

w.small=rnorm(50,mean=0,sd=1)
w.small=as.ts(w.small)
acf.w.small=acf(w.small,lag.max=20,type="correlation")

x=filter(x,filter=c(1/3,3),method="recursive",sides=2)



par(mfcol = c(3,2)) 
for (i in 1:6){
x = ts(cumsum(rnorm(100,.01,1))) 
reg = lm(x~0+time(x), na.action=NULL)
plot(x) 
lines(.01*time(x), col="red", lty="dashed") 
abline(reg, col="blue") } 

library("astsa")

### Detrended
fit = lm(gtemp~time(gtemp), na.action=NULL)
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
plot(diff(gtemp), type="o", main="first difference")
par(mfrow=c(3,1))
acf(gtemp, 48, main="gtemp")
acf(resid(fit), 48, main="detrended")
acf(diff(gtemp), 48, main="first difference")


### moving average
ma5=filter(gtemp, sides=2, rep(1,5)/5,method="convolution")
ma30=filter(gtemp, sides=2, rep(1,30)/30,method="convolution")
plot(gtemp, type="p", ylab="global temperature deviations",main="Moving Average Smoother")
lines(ma5)
lines(ma30,col="red",lwd=3)


