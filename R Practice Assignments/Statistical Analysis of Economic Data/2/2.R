rm(list=ls())
require(foreign)
unionData <- read.dta("http://www.sfu.ca/~cmuris/2014-Summer-333/wagepan.dta")
unionData <- na.omit(unionData)

str(unionData)
summary(unionData)
head(unionData)

timestamp()
getwd()

unionData$wage<-exp(unionData$lwage)
unionData$wage

require(ggplot2)
qplot(union,wage,data=unionData)
qplot(union,lwage,data=unionData)

fit1=lm(wage~union,data=unionData)
summary(fit1)
confint(fit1,'union',level=0.90)

fit2=lm(lwage~union,data=unionData)
summary(fit2)
confint(fit2,'union',level=0.90)

fit3=lm(lwage~union+hours+year+occ1+occ2+occ3+occ4+occ5+occ6+occ7+occ8+occ9,data=unionData)
summary(fit3) 

fit4=lm(lwage~union+hours+year+occ1+occ2+occ3+occ4+occ5+occ6+occ7+occ8+exper,data=unionData)
summary(fit4)

fit5=lm(lwage~union+hours+year+occ1+occ2+occ3+occ4+occ5+occ6+occ7+occ8+exper+educ,data=unionData)
summary(fit5)

fit6=lm(exper~educ,data=unionData)
summary(fit6)
