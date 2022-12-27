#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 9, Part 2  - Cereal
#
#  Read in the accidents data, Practice how to select the 
#  samples. And then check if the log(se) of statistics 
#  decline as log(n) with n^(-2)
#  
#  Change log:
#    2015-03-16 KY First Edition
#
#*******************************************************

# Assignment 9, Part 2 - Analysis of accidents data

# clear the workspace then set & show the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 9/Accident")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(plyr)
library(reshape2)
library(ggplot2)

# read in the data 
radf <- read.csv("road-accidents-2010.csv", as.is=TRUE, strip.white=TRUE, 
                      header=TRUE, fill=TRUE)

# remove the missing value (NA)  
radf[radf==-1] <- NA
radf <- radf[!is.na(radf$Number_of_Vehicles),]
radf <- radf[!is.na(radf$Number_of_Casualties),]
radf <- radf[!is.na(radf$Accident_Severity),]


# create the variable AccidentIndex
radf <- transform(radf, AccidentIndex=Number_of_Vehicles*Number_of_Casualties/Accident_Severity)

# print the first 10 records after fixedup
radf[1:10,]


#-----------------------set the function---------------------------------
# Select a random sample of size 10 from the accident data frame
my.sample <- sample(1:nrow(radf), size=10, replace=FALSE)
my.sample


# function containing the sample size, the mean and the 90th percentile 
# of a sample of the accident index values
AI.stat <- function(radf, sample.size){
  my.sample <- sample(1:nrow(radf), size=sample.size, replace=FALSE)
  sampled.acc.index <- radf$AccidentIndex [my.sample]
  mean <- mean(sampled.acc.index)
  per90 <- quantile(sampled.acc.index, .90)
  res<- c(sample.size, mean, per90)
  names(res) <- c("sample.size", "mean","per90")
  return(res)
}


# Check the function
AI.stat(radf, 10)
AI.stat(radf, 10)
AI.stat(radf,100)
AI.stat(radf,100)


# repeatedly apply the function
my.result <- rdply(5, AI.stat(radf=radf, sample.size=10), .id="rep")
my.result



#-----------------------SE of the statistics-----------------------------------
# Create a new function that compute the sd of the mean and per90 values
AI.sim <- function (sample.size, radf, nsamples=1000){
  # After all samples are generated, compute the sd of the mean and per90 values
  my.result <- rdply(nsamples, AI.stat(radf=radf, sample.size=sample.size), .id="rep")
  se.mean <- sd(my.result$mean)
  se.per90 <- sd(my.result$per90)
  res<- c(sample.size, se.mean, se.per90)
  names(res) <- c("sample.size","se.mean","se.per90")
  return(res)
}

# check the new function
AI.sim(sample.size=20, radf=radf, nsamples=100)
AI.sim(sample.size=20, radf=radf, nsamples=100)
AI.sim(sample.size=200, radf=radf, nsamples=100)
AI.sim(sample.size=2000, radf=radf, nsamples=100)


# call the AI.sim() function with sample sizes 100,200,400,1000,2000 and 4000
my.result <- ldply(c(100,200,400,1000,2000,4000), AI.sim, radf=radf, nsamples=1000)
my.result



#------------------check the slopes-----------------------------------
# check if the log(se) of statistics decline as log(n)
# Create derived variables for the log(sample size), log(se p90), and log(se mean).
my.result$log.sample.size = log(my.result$sample.size)
my.result$log.se.mean = log(my.result$se.mean)
my.result$log.se.per90 = log(my.result$se.per90)
my.result

# Fit a line to the relationship between the log(sample size) and the log(se) for the two statistics.
lm.mean <- lm(log.se.mean ~ log.sample.size, data=my.result)
coef(lm.mean)
confint(lm.mean, 'log.sample.size', level=0.95)

lm.per90 <- lm(log.se.per90 ~ log.sample.size , data=my.result)
coef(lm.per90)
confint(lm.per90, 'log.sample.size', level=0.95)


#--------------------make plot-----------------------------------------------
# converts from wide-format to long-format
my.result2 <-melt(my.result[1:3], id="sample.size")
my.result2

# plot the relationship between the log(se) of each statistic and log(n).
plot <- ggplot(data=my.result2, aes(x=log(sample.size), y=log(value),
                            group=variable, color=variable, shape=variable, linetype=variable))+
  ggtitle("The relationship between the log(se) of each statistic and log(n)")+
  xlab("Log(n)")+ylab("Log(se) of each statistic")+
  geom_point()+
  geom_line ()+
  geom_smooth(method="lm",se=FALSE)
plot

# save the plot and table
ggsave(plot, file='plot.png', h=4, w=6, units="in", dpi=200)




