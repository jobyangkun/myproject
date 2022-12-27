#*****************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 6, Part 2  - Titanic
#
#  Read in the Titanic dataset, check how many survived
#  and dies, and then plot the log-odds ratio and probability
#  of survival by passenger class and sex.
#
#   Change log:
#    2015-02-25 KY First Edition
#
#****************************************************

# Assignment 6, Part 2 - Analysis of Titanic data

# clear the workspace then set & show the current working directory.
setwd("C:/Users/Kun/Desktop/340/R/Assignment 6/titanic")
getwd()
rm(list=ls())

# read in the data and check the stucture of the object.
titanic <- read.table(url("http://www.statsci.org/data/general/titanic.txt"),
           sep='\t', header=TRUE, fill=TRUE, as.is=TRUE, strip.white=TRUE)

# verify that the data was read in properly.
str(titanic)
dim(titanic)
head(titanic)
tail(titanic)

# create a summary table
with(titanic, table(PClass, Sex, Survived, useNA='always'))
xtabs(~Survived+Sex+PClass, data=titanic)



# total number of passenger that survived or died over all sexes and PClass 
overall.surviv <- with(titanic, table(Survived))
overall.surviv

# Comparison of proportions 
overall.prob.death <- overall.surviv[1]/sum(overall.surviv)
cat("Overall prob of death ", overall.prob.death, "\n")

# comparisons of odds ratio
overall.odds.death<- overall.surviv[1]/overall.surviv[2]
cat('Overall Odds of death:',overall.odds.death,'\n')

# comparisons of log(odds ratio)
overall.logodds.death<- log(overall.odds.death)
cat('Overall log-Odds of death:',overall.logodds.death,'\n')



# fit the glm() model of Survived
overall.glm <- glm(Survived ~ 1, data=titanic, family=binomial(link=logit))
summary(overall.glm)

# Extract the coefficient.
coef(overall.glm) 

# the confidence intervals for the intercept.
confint(overall.glm)



# Put them together into one vector
overall.logodds.glm <- c(coef(overall.glm),confint(overall.glm))
cat("Estimate of overall log-odds of survival",
       "and 95 ci from glm",overall.logodds.glm,"\n")

# estimate the odds (on the anti-log) scale.
overall.odds.glm <- exp(overall.logodds.glm)
cat("Estimate of overall odds of survival",
    "and 95 ci from glm",overall.odds.glm,"\n")

# convert the estimated odds to the original probability
overall.prob.glm<- overall.odds.glm/(1+overall.odds.glm)
cat("Estimate of overall probability of survival",
    "and 95 ci from glm",overall.prob.glm,'\n')



# create a new factor variable (PClassF & SexF)
titanic$PClassF <- as.factor(titanic$PClass)
titanic$SexF <- as.factor(titanic$Sex)
str(titanic)

# fit the two-factor glm() model
sex.pass.glm <- glm(Survived ~ SexF+PClassF + SexF:PClassF,
                    data=titanic, family=binomial(link=logit))


# ANODEV table (Type I (incremental) rather than Type III (marginal) tests)
# only the interaction result is useful
anova(sex.pass.glm, test='Chi')




library(lsmeans)
# get the marginal estimates of the log-odds 
sex.pass.glm.lsmo <- lsmeans::lsmeans(sex.pass.glm, ~ SexF:PClassF)
sex.pass.glm.est.logodds <- summary(sex.pass.glm.lsmo, type="link")
sex.pass.glm.est.logodds

# get the marginal estimates of the probability)
sex.pass.glm.est.p <- summary(sex.pass.glm.lsmo, type="response")
sex.pass.glm.est.p



library(ggplot2)
# Make the logodds plot using ggplot
logodds.plot <-
  ggplot(data=sex.pass.glm.est.logodds,
         aes(x=PClassF, y=lsmean, group=SexF, shape=SexF, color=SexF))+
  ggtitle("Comparison of logodds of survival by sex and passenger class")+
  xlab("Passenger Class")+ylab("log(Odds) survival and 95% CI")+
  geom_point()+
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),width=0.2)+
  geom_line()
logodds.plot
ggsave(logodds.plot,file='assign06-part02-logodds.png',h=4,w=6,units="in",dpi=200)

# Make the probability of survival plot
prob.plot <-
  ggplot(data=sex.pass.glm.est.p,
         aes(x=PClassF, y=prob, group=SexF, shape=SexF, color=SexF))+
  ggtitle("Comparison of probability of survival by sex and passenger class")+
  xlab("Passenger Class")+ylab("probability of survival and 95% CI")+
  geom_point()+
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),width=0.2)+
  geom_line()
prob.plot

