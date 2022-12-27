#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 10, Part 2  - Cellphone
#
#  Read in the Cellphone data. Examine the probability
#  of a cell phone interrupting the movie for each ad.
#  Check the effect of heavy breathing on having all
#  cell phones turned off relative to no advertisement.
#  
#  Change log:
#    2015-03-24 KY First Edition
#
#*******************************************************

# Assignment 10, Part 02 - Analysis of Cellphone data

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 10/cellphone")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(car)
library(lsmeans)
library(ggplot2)

# read in the data
cellphone = read.table("cellphone.txt", sep='', header=TRUE, fill=TRUE, as.is=TRUE)
cellphone[1:10,]




# Convert the number of cell phones that went off to a simple yes/no variable.
cellphone$interrupt <- recode(cellphone$calls, "0='no'; else='yes'")
xtabs(~calls+interrupt, data=cellphone)


# Reorder the ad factor to have the control group
cellphone$new.ad <- recode(cellphone$ad, "'none'='control'")
xtabs(~ad+new.ad, data=cellphone)


# convert the new variables to factors
cellphone$interruptF <- factor(cellphone$interrupt, levels=c("no","yes"), ordered=TRUE)
cellphone$new.adF <- factor(cellphone$new.ad, levels=c("control","dh","dv","jc","ss"))



# Fit a suitable model for this data
my.glm <- glm(interruptF ~ new.adF, data=cellphone, family=binomial(link=logit))
summary(my.glm)

# Test the hypothesis that the probability of a cell phone interruption is the same for all ads
anova(my.glm, test="Chisq")

# Estimate the probability of a cell phone interrupting the movie for each ad
lsmo <- lsmeans:::lsmeans(my.glm, ~ new.adF, adust="tukey")
report.lsmeans <- summary(lsmo)
report.lsmeans

# get the probability and CI
report.lsmeans$logodds  <- report.lsmeans$lsmean
report.lsmeans$odds     <- exp(report.lsmeans$logodds)
report.lsmeans$odds.lcl <- exp(report.lsmeans$asymp.LCL)
report.lsmeans$odds.ucl <- exp(report.lsmeans$asymp.UCL)
report.lsmeans$prob     <- report.lsmeans$odds/(1+report.lsmeans$odds)
report.lsmeans$prob.lcl <- report.lsmeans$odds.lcl/(1+report.lsmeans$odds.lcl)
report.lsmeans$prob.ucl <- report.lsmeans$odds.ucl/(1+report.lsmeans$odds.ucl)
report.lsmeans


# create a plot for it 
plot001 <- ggplot(data=report.lsmeans, aes(x=new.adF, y=prob))+
  ggtitle("Probability of a cell phone interrupting the movie")+
  xlab("Advertisements")+ylab("Probability of interrupting")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=prob.lcl, ymax=prob.ucl), width=0.2)
plot001



# Estimate the difference in the marginal means & an approximate 95% confidence interval
pairs(lsmo)
diff.lsmo <- confint(pairs(lsmo))
diff.lsmo

# Convert this to an odds ratio along with a 95% confidence interval
diff.lsmo$diff.logodds  <- diff.lsmo$estimate
diff.lsmo$odds.ratio    <- exp(diff.lsmo$diff.logodds)
diff.lsmo$odds.lcl      <- exp(diff.lsmo$asymp.LCL)
diff.lsmo$odds.ucl      <- exp(diff.lsmo$asymp.UCL)
diff.lsmo[c(1,7:10)]

# save the plot
ggsave(plot=plot001, file='plot.png', h=4, w=6, units='in', dpi=300)



