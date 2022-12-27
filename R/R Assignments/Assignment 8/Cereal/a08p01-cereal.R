#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 8, Part 1  - Cereal
#
#  Read in the cereal dataset, create two bar chart of  
#  mean calories by fiber class. Examine if the 
#  mean calories are same across fiber class.  
# 
#  Change log:
#    2015-03-11 KY First Edition
#
#*******************************************************

# Assignment 8, Part 1 - Analysis of cereal data

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 8/Cereal")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(car)
library(ggplot2)
library(gridExtra)
library(plyr)
library(lsmeans)

# read in the data & replace -1 to missing value (NA)
cereals <- read.csv("cereal.csv", as.is=TRUE, strip.white=TRUE, 
                    header=TRUE, fill=TRUE)
cereals[cereals==-1] <- NA
any(cereals==-1, na.rm=TRUE)

# List the first 10 records after fixups
cereals[1:10,]



# Make a histogram of the grams of fibre/serving
histogram <- ggplot(data=cereals, aes(x=fiber))+
  ggtitle("The histogram of the grams of fibre/serving")+
  xlab("fibre/serving")+ylab("Number of cereals")+
  geom_histogram(binwidth=0.5, alpha=0.3)
histogram

# Find the five number summary
summary(cereals$fiber)

# Create a categorical variable-fiberclass
cereals$fiberclass <- recode(cereals$fiber,
                             "lo:2='low'; 2:hi='high'; else=NA")

# make a dot plot to check the categorical variable
fiberclass.check <- ggplot(data=cereals, aes(x=fiberclass, y=fiber))+
  ggtitle("fiber by class")+
  xlab("fiberclass")+ylab("fiber per serving (g)")+
  geom_jitter(position=position_jitter(h=0, w=0.3))
fiberclass.check

# Convert fiberclass variable to a factor
cereals$fiberclassF <- factor(cereals$fiberclass, levels=c("low","high"))
levels(cereals$fiberclassF)
str(cereals)



#---------------------------------------------------------------------
# examine if the mean calories/serving varies by fiber class
# make a side-by-side box-plot with an notched box plot 
notched.box<- ggplot(data=cereals, aes(x=fiberclassF, y=calories))+
  ggtitle("calories by fiber class")+
  xlab("Fiber class")+ylab("Calories/serving")+
  geom_boxplot(notch=TRUE, alpha=0.5)+
  geom_jitter(position=position_jitter(h=0.2, w=0.1))
notched.box

# compute the mean calories for each fibre class along with its SE and a 95% CI
fiberclass.report <- ddply(cereals, "fiberclassF",
                           function(x){
                             fit <- lm(x$calories ~1, data=cereals)
                             mean <- coef(fit)
                             se <- sqrt(diag(vcov(fit)))
                             ci <- confint(fit)
                             res <- c(mean, se, ci)
                             names(res) <- c("mean", "mean.se","lcl",'ucl')
                             return(res)
                             })
fiberclass.report


# bar chart of the estimated mean calories with the 95% CI
plot.means <- ggplot(data=fiberclass.report, aes(x=fiberclassF, y=mean))+
  ggtitle("The estimated mean calories by fiber class with 95% CI")+
  xlab("Fiber class")+ylab("Mean calories")+
  geom_bar(stat="identity", alpha=.5, w=0.3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.05)
plot.means



#---------------------------------------------------------------------
# using t.test to check equality of population mean calories between fiber classes
fiberclass.ttest.object <- t.test(calories ~ fiberclassF, data=cereals)
str(fiberclass.ttest.object)

# estimated difference in means
fiberclass.ttest.object$estdiff <- sum(c(1,-1)*fiberclass.ttest.object$estimate)
class(fiberclass.ttest.object)

# estimated se of difference in means
fiberclass.ttest.object$estdiff.se <- fiberclass.ttest.object$estdiff/fiberclass.ttest.object$statistic

# Print the object and estimated difference in means (with se)
print(fiberclass.ttest.object)
print(fiberclass.ttest.object$estdiff)
print(fiberclass.ttest.object$estdiff.se)



#---------------------------------------------------------------------____
# use lm() to compare the mean of calories/serving between fiber classes
fiberclass.lm <- lm(calories ~ fiberclassF, data=cereals)

# Use the anova() to get the formal test (p-value)
anova(fiberclass.lm)

# get the coefficients from the summary
summary(fiberclass.lm)



#---------------------------------------------------------------------
# produces the marginal means and 95% CI
lsmeans <- lsmeans::lsmeans(fiberclass.lm, ~fiberclassF, adjust="tukey")

# compare the estimated marginal means with previous output
summary (lsmeans)
fiberclass.report

# create a bar chart of the merginal mean and CI
lsmeans2 <- summary(lsmeans, digits=2)
plot.means2 <- ggplot(data=lsmeans2, aes(x=fiberclassF, y=lsmean))+
  ggtitle("The estimated merginal mean calories by fiber class with 95% CI")+
  xlab("Fiber class")+ylab("Merginal mean calories")+
  geom_bar(stat="identity", alpha=.5, w=0.3)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.05)
plot.means2

# compare the the difference in the estimated marginal mean and its SE with the previous output
pairs (lsmeans)
print(fiberclass.ttest.object$estdiff)
print(fiberclass.ttest.object$estdiff.se)
summary(fiberclass.lm)

# save the plot
ggsave(plot.means, file='plot.means.png', h=4, w=6, units="in", dpi=200)
ggsave(plot.means2, file='plot.means2.png', h=4, w=6, units="in", dpi=200)



#-------------------------------------------------------------------------------
# produce diagnostic plots
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
sf.autoplot.lm(fiberclass.lm)



