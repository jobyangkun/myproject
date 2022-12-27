## Stat475, Spring 2014, Assignment 6
# Q 5.18
data5.18 <- expand.grid(city=factor(c("Beijing", "Shanghai", "Shenyang","Nanjing","Harbin","Zhengzhou","Taiyuan","Nanchang")),smoke=factor(c('Y','N')))
data5.18 <- data.frame(data5.18,yes=c(126,908,913,235,402,182,60,104,35,497,336,58,121,72,11,21),no=c(100,688,747,172,308,156,99,89,61,807,598,121,215,98,43,36))
# (a)
logit.fit <- glm(cbind(yes,no)~smoke+city,data=data5.18,family=binomial(link=logit))
summary(logit.fit)
# (b)
res <- resid(logit.fit, type = "pearson")
sum(res^2)
1-pchisq(sum(res^2),df=logit.fit$df.residual)
# (c)
res.std <- rstandard(logit.fit,type="pearson")

# Q 7.6
data7.6 <- read.table("data7.6.txt", header=TRUE, sep="")
# mutually indep model
fit1 <- glm(count~EI+SN+TF+JP, family=poisson, data=data7.6) 
summary(fit1)
# (a)
1-pchisq(fit1$deviance,df=fit1$df.residual) # G^2 test of goodness-of-fit
# homo asso model
# (b) (c)
fit2<-glm(count~.^2, family=poisson, data=data7.6) 
summary(fit2)
