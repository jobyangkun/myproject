#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 8, Part 2  - Cereal
#
#  Read in the accidents data, test hypotheses.Examine 
#  if there is evidence of a difference in the odds of fatality 
#  among the various hours.Create the plot of fatality rate. 
#  
#  Change log:
#    2015-03-11 KY First Edition
#
#*******************************************************

# Assignment 8, Part 2 - Analysis of accidents data

# clear the workspace then set & show the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 8/Accident")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(car)
library(ggplot2)
library(gridExtra)
library(plyr)
library(lsmeans)

# read in the data 
accidents <- read.csv("road-accidents-2010.csv", as.is=TRUE, strip.white=TRUE, 
                 header=TRUE, fill=TRUE)

# replace -1 to missing value (NA)  
accidents[accidents==-1] <- NA

# print the first 10 records after fixedup
accidents[1:10,]

# convert the Date and Time to dates and times
accidents$DateTime <- as.POSIXct(paste(accidents$Date," ",accidents$Time), 
                            format="%d/%m/%Y %H:%M", tz="UTC")

# check the missing values
sum(is.na(accidents$DateTime))
accidents[is.na(accidents$DateTime), c("Date","Time", "DateTime")]

# find the minimum, maximum, and other statistics about the date-time
summary(accidents$DateTime)

# Extract the hour of the accident
accidents$hour <- as.numeric(format(accidents$DateTime, "%H"))

# Create a categorical variable-fiberclass
accidents$fatal <- recode(accidents$Accident_Severity,
                             "1='Yes';2:3='No';else=NA")

# Create a fatal factor 
accidents$fatalF <- factor(accidents$fatal, levels=c("No","Yes"))
xtabs(~fatalF+Accident_Severity, data=accidents)
str(accidents$fatalF)

# Create a table of the fatality status by the hour
xtabs(~fatalF+hour, data=accidents)

# delete the rows have a missing value for the hour
sum(is.na(accidents$hour))
accidents.new <- accidents[!is.na(accidents$hour),]

# Create a hour factor 
accidents.new$hourF <- as.factor(accidents.new$hour)


#-------------------------------------------------------------------
# estimate of the log-odds along with its SE and CI
glm.log.odds <- ddply(accidents.new, "hourF",
                      function(x){
                        fit <- glm(x$fatalF ~1, data=accidents.new, family=binomial)
                        log.odds <- coef(fit)
                        se <- sqrt(diag(vcov(fit)))
                        ci <- confint(fit)
                        res <- c(log.odds, se, ci)
                        names(res) <- c("log.odds","se","lcl",'ucl')
                        return(res)
                        })
glm.log.odds

#-------------------------------------------------------------------
# estimate of the odds along with its SE and CI
glm.odds <- ddply(accidents.new, "hourF",
                   function(x){
                   fit <- glm(x$fatalF ~1, data=accidents.new, family=binomial)
                   odds <- exp(coef(fit))
                   ci <- exp(confint(fit))
                   res <- c(odds, ci)
                   names(res) <- c("odds","lcl",'ucl')
                   return(res)
                   })
glm.odds

# Plot the odds of fatality and the 95% confidence interval by the hour
plot.odds <- ggplot(data=glm.odds, aes(x=hourF, y=odds))+
  ggtitle("The odds of fatality and the 95% CI by the hour")+
  xlab("Hour")+ylab("The odds of fatality & 95% CI")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.4)
plot.odds


#-------------------------------------------------------------------
# estimate of the probability along with its SE and CI
prob <- ddply(accidents.new, "hourF",
                 function(x){
                   fit <- glm(x$fatalF ~1, data=accidents.new, family=binomial)
                   prob <- exp(coef(fit))/(1+exp(coef(fit)))
                   ci <- exp(confint(fit))/(1+exp(confint(fit)))
                   res <- c(prob, ci)
                   names(res) <- c("prob","lcl",'ucl')
                   return(res)
                 })
prob


#---------------------------------------------------------------------
# Use the glm() function to model the fatality rate across the hours
fatality.glm <- glm(fatalF ~hourF, data=accidents.new, family=binomial)

# examine the results of the model.
summary(fatality.glm)

# examine if different in the log-odds of fatality among the various hours
anova.glm(fatality.glm, test="LRT")



#-----------------------------------------------------------------------
# use lamean to produce the marginal estimates (of the log-odds) and 95% CI
lsmeans <- lsmeans::lsmeans(fatality.glm, ~hourF, adjust="tukey")

# get the SE & CI then compare with the previous output
summary(lsmeans)
glm.log.odds

# save the output from the lsmeans calls
output.lsmeans <- summary(lsmeans)

output.lsmeans$odds=exp(output.lsmeans$lsmean)
output.lsmeans$LCL=exp(output.lsmeans$asymp.LCL)
output.lsmeans$UCL=exp(output.lsmeans$asymp.UCL)
output.lsmeans

# make a plot of the marginal estimates (of the odds) and 95% CI
plot.lsmeans<- ggplot(data=output.lsmeans, aes(x=hourF, y=odds))+
  ggtitle("The odds of fatality and the 95% CI by the hour")+
  xlab("Hour")+ylab("The odds of fatality & 95% CI")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL), w=0.4)
plot.lsmeans

# save the plot
ggsave(plot.odds, file='plot.means1.png', h=4, w=6, units="in", dpi=200)
ggsave(plot.lsmeans, file='plot.means2.png', h=4, w=6, units="in", dpi=200)




