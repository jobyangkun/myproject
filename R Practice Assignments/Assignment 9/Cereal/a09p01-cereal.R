#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 9, Part 1  - Cereal
#
#  Read in the cereal dataset, Get the basic statistics of
#  different variables. Do bootstrapping in R.
#  Get the SE for the statistics by bootstrapping.
# 
#  Change log:
#    2015-03-16 KY First Edition
#
#*******************************************************

# Assignment 9, Part 1 - Analysis of cereal data

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 9/Cereal")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(boot)
library(plyr)

# read in the data & replace -1 to missing value (NA)
cereals <- read.csv("cereal.csv", as.is=TRUE, strip.white=TRUE, 
                    header=TRUE, fill=TRUE)
cereals[cereals==-1] <- NA
any(cereals==-1, na.rm=TRUE)

# List the first 10 records after fixups
cereals[1:10,]



#-----------------------------------------------------------
# set the function computes mean, median, SD, Gini SD, se, 95% CI
my.est <- function(x){
  n       <- length(x)
  mean    <- mean(x,na.rm=TRUE)
  median  <- median(x,na.rm=TRUE)
  sd      <- sd(x, na.rm=TRUE)
  gsd     <- (sum(abs(outer(x,x,FUN="-")),na.rm=TRUE)/n/(n-1))*
             (sqrt(pi)/2)
  se      <- sd/sqrt(length(na.omit(x)))
  lcl     <- mean-qt(0.975,length(na.omit(x))-1)*se
  ucl     <- mean+qt(0.975,length(na.omit(x))-1)*se
  res     <- c(mean,median,sd,gsd,se,lcl,ucl)
  names(res)<- c('mean','median','sd','gsd','se.mean','95%.lcl.mean','95%.ucl.mean')
  return(res)
}

# Test the function
my.est(cereals$calories)
my.est(cereals$sugars)




#---------------function by bootstrap---------------------
# set the function computes mean, median, SD, Gini SD by bootstrap
my.est2 <- function(data, indices){
  data    <- data[indices]
  n       <- length(data)
  mean    <- mean(data,na.rm=TRUE)
  median  <- median(data,na.rm=TRUE)
  sd      <- sd(data, na.rm=TRUE)
  gsd     <- (sum(abs(outer(data,data,FUN="-")),na.rm=TRUE)/n/(n-1))*
             (sqrt(pi)/2)
  res     <- c(mean,median,sd,gsd)
  names(res)<- c("mean","median","sd","gsd")
  return(res)
}



#-----------------------------Calories-----------------------------
# generate bootstrap results
my.boot.results.calories <- boot(cereals$calories, statistic=my.est2, R=1000)

# check the attributes of the result
str(my.boot.results.calories)

# List the t0 object
list(my.boot.results.calories$t0)

# List the first 10 records of the t object
list(my.boot.results.calories$t[1:10,])

# Print the results
print(my.boot.results.calories)


#-----------------Calories----------------------------------------------
# plot the Calories Sample Mean
plot(my.boot.results.calories, index=1)
title(main='Calories Sample Mean', line=3)

# plot the Calories Sample median
plot(my.boot.results.calories, index=2)
title(main='Calories Sample Median', line=3)

# plot the Calories Sample SD
plot(my.boot.results.calories, index=3)
title(main='Calories Sample Standard Deviation', line=3)

# plot the Calories Sample Gini SD
plot(my.boot.results.calories, index=4)
title(main='Calories Sample Gini Standard Deviation', line=3)

# check the particular class associated with the object
class(my.boot.results.calories)

# check the methods associated with the object
methods(class=class(my.boot.results.calories))


#----------------Calories-------------------------------------------
# Sample mean percentile ci from bootstrapping
boot.ci(my.boot.results.calories, index=1, type="perc")

# Sample median percentile ci from bootstrapping
boot.ci(my.boot.results.calories, index=2, type="perc")

# Sample Standard Deviation percentile ci from bootstrapping
boot.ci(my.boot.results.calories, index=3, type="perc")

# Sample Gini SD percentile ci from bootstrapping
boot.ci(my.boot.results.calories, index=4, type="perc")





#--------------------SUGARS-------------------------------------------
# generate bootstrap results for the sugars/serving
my.boot.results.sugars <- boot(cereals$sugars, statistic=my.est2, R=1000)

# List the t0 object
list(my.boot.results.sugars$t0)

# List the first 10 records of the t object
list(my.boot.results.sugars$t[1:10,])

# Print the results
print(my.boot.results.sugars)


#---------------------SUGARS-----------------------------------
# plot the Sugars Sample Mean
plot(my.boot.results.sugars, index=1)
title(main='Sugars Sample Mean', line=3)

# plot the Sugars Sample median
plot(my.boot.results.sugars, index=2)
title(main='Sugars Sample Median', line=3)

# plot the Sugars Sample SD
plot(my.boot.results.sugars, index=3)
title(main='Sugars Sample Standard Deviation', line=3)

# plot the Sugars Sample Gini SD
plot(my.boot.results.sugars, index=4)
title(main='Sugars Sample Gini Standard Deviation', line=3)


#-------------------SUGARS-----------------------------------
# Sample mean percentile ci from bootstrapping
boot.ci(my.boot.results.sugars, index=1, type="perc")

# Sample median percentile ci from bootstrapping
boot.ci(my.boot.results.sugars, index=2, type="perc")

# Sample Standard Deviation percentile ci from bootstrapping
boot.ci(my.boot.results.sugars, index=3, type="perc")

# Sample Gini SD percentile ci from bootstrapping
boot.ci(my.boot.results.sugars, index=4, type="perc")


#--------------------------table-------------------------------------------------
# extract the results
est <- my.est(cereals$sugars)[1:4]
se <- c(my.est(cereals$sugars)[5], NA, NA, NA)
lcl <- c(my.est(cereals$sugars)[6], NA, NA, NA)
ucl <- c(my.est(cereals$sugars)[7], NA, NA, NA)


boot.se <- aaply(my.boot.results.sugars$t, 2, sd)
boot.mean.ci <- boot.ci(my.boot.results.sugars, index=1, type="perc")$percent[,4:5]
boot.median.ci <- boot.ci(my.boot.results.sugars, index=2, type="perc")$percent[,4:5]
boot.sd.ci <- boot.ci(my.boot.results.sugars, index=3, type="perc")$percent[,4:5]
boot.Gini.sd.ci <- boot.ci(my.boot.results.sugars, index=4, type="perc")$percent[,4:5]

boot.lcl<-c(boot.mean.ci[1],boot.median.ci[1], boot.sd.ci[1], boot.Gini.sd.ci[1])
boot.ucl<-c(boot.mean.ci[2],boot.median.ci[2], boot.sd.ci[2], boot.Gini.sd.ci[2])


# Create a table
table = round(data.frame(est, se,lcl, ucl,
                         boot.se, boot.lcl, boot.ucl),2)

colnames(table) <- c("Estimate", "SE", "LCL", "UCL",
                     "Boot.SE", "Boots.LCL",
                     "Boot.UCL")
table


# save the table
sink('table1.txt', split=TRUE)
table
sink()

