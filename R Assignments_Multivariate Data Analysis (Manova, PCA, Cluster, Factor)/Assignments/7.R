### the median
qnorm(0.5, mean=2, sd=1)
### the two quartiles
qnorm(0.25, mean=2, sd=1)
qnorm(0.75, mean=2, sd=1)



### the two whiskers
IQR<-qnorm(0.75, mean=2, sd=1)-qnorm(0.25, mean=2, sd=1)
qnorm(0.75, mean=2, sd=1)+1.5*IQR
qnorm(0.25, mean=2, sd=1)-1.5*IQR



lowerw<-qnorm(0.25, mean=2, sd=1)-1.5*IQR
pnorm(lowerw, mean=2, sd=1)
upperw<-qnorm(0.75, mean=2, sd=1)+1.5*IQR
1-pnorm(upperw, mean=2, sd=1)
pnorm(lowerw, mean=2, sd=1)+(1-pnorm(upperw, mean=2, sd=1))



p<-pnorm(lowerw, mean=2, sd=1)+(1-pnorm(upperw, mean=2, sd=1))
dbinom(1, 20, p)