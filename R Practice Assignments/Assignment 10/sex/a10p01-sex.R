#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 10, Part 1  - Sex
#
#  Input the data and set a function for log-likelihood.
#  Get the MLE and the 95%  confidence interval by using nlm
#  and mle2. Finally, create a plot for the log-likelihood 
#  function with 95% CI
#  
#  Change log:
#    2015-03-24 KY First Edition
#
#*******************************************************

# Assignment 10, Part 01 - Analysis of Sex

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 10/sex")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(plyr)
library(ggplot2)
library(bbmle)

# Create a data frame that has two variables 
my.data <- data.frame(
  months = c(2,7,5,0,0,3,0,4,10),
  censored=c(0,0,0,0,0,0,0,0,1))
my.data


# set a function for log-likelihood
log.like <- function(p, data, return.negll=FALSE){
  log.like <- sum(data$months*log(1-p))+log(p)*sum(data$censored==0)
  if(return.negll){log.like <- -log.like}
  return(log.like)
}


# Test the function
log.like (.2, my.data)
log.like (.2, my.data, return.negll=TRUE)



#-------------------plot the basic log-likelihood function-----------------------------
# set the range of the parameter
lambda <- seq(0,0.5,.01)
lambda[1:10]

# make plot
plot.points <- laply(lambda, log.like, data=my.data)
plot(lambda, plot.points, type="l")


#----------------------MLE,CI---by using nlm-----------------------------------------
# find the MLE
mle <- nlm(log.like, 0.2, data=my.data, return.negll=TRUE, hessian=TRUE)


# find the standard error
mle$se <- sqrt(diag(solve(mle$hessian)))

# Find the large sample confidence limits
mle$alpha <- .05
mle$ci    <- c(LCL=mle$estimate - qnorm(1-mle$alpha/2)*mle$se, 
               UCL=mle$estimate + qnorm(1-mle$alpha/2)*mle$se)
mle


#----------------------MLE,CI---by using nlm2-----------------------------------------
# repeat the optimization using the mle2()
mle2 <- mle2(log.like, list(p=0.2), data=list(data=my.data, return.negll=TRUE))

# many methods available
summary(mle2)
coef(mle2)
sqrt(diag(vcov(mle2)))
mle2.ci <- confint(mle2)
mle2.ci



# make the final plot
plotdata <- data.frame(lambda=lambda, log.like=plot.points)
plot001 <- ggplot(data=plotdata, aes(x=lambda, y=log.like))+
  ggtitle("Log likelihood function")+
  xlab("Lambda")+ylab("log-likelihood")+
  geom_line()+
  annotate("point", x=mle$estimate, y=-mle$minimum, color="red", size=5)+
  annotate("point", x=mle$ci["LCL"],y=log.like(mle$ci["LCL"],data=my.data), 
           color="blue", size=5, shape=2)+
  annotate("point", x=mle$ci["UCL"],y=log.like(mle$ci["UCL"],data=my.data), 
           color="blue", size=5, shape=2)+
  annotate("point", x=mle2.ci["2.5 %"],y=log.like(mle2.ci["2.5 %"],data=my.data), 
           color="gold", size=5, shape=3)+
  annotate("point", x=mle2.ci["97.5 %"],y=log.like(mle2.ci["97.5 %"],data=my.data), 
           color="gold", size=5, shape=3)+
  annotate("text", x=mle$estimate, y=-mle$minimum - 10,label=paste("MLE:", round(mle$estimate,3)))+
  annotate("text", x=mle$estimate, y=-mle$minimum - 11, label=paste("SE  :", round(mle$se,3)))+
  annotate("text", x=mle$estimate, y=-mle$minimum - 12, 
           label=paste("A CI (nlm):(",paste(round(mle$ci,3),sep=",",collapse=","),")",sep=""))+
  annotate("text", x=mle$estimate, y=-mle$minimum - 13, 
           label=paste("A CI (mle2):(",paste(round(mle2.ci,3),sep=",",collapse=","),")",sep=""))
plot001

# save plot
ggsave(plot=plot001, file='like.plot.png', h=4, w=6, units='in', dpi=300)

