rm(list=ls())
setwd("C:\\Users\\Kun\\Desktop\\homework")
data1<-read.csv("Table7.21.csv",header=TRUE,sep=",")

loglinear1 <-glm(Count~EI+SN+JP+TF,family=poisson, data=data1)
summary(loglinear1)
pearson <- sum(resid(loglinear1, type="pearson")^2)
pearson 

loglinear2<-glm(Count~EI*SN+EI*JP+EI*TF+SN*JP+SN*TF+JP*TF,family=poisson, data=data1)
summary(loglinear2)
pearson <- sum(resid(loglinear2, type="pearson")^2)
pearson 

rm(list=ls())
setwd("C:\\Users\\Kun\\Desktop\\homework")
data2<-read.csv("table5.12.csv",header=TRUE,sep=",")
data2.lm <-glm(cbind(yes,no)~city+smoking,family=binomial, data=data2)
summary(data2.lm)


pearson <- sum(resid(data2.lm, type="pearson")^2)
pearson
df.residual(data2.lm)
