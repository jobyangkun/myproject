#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 7, Part 1  - Cereal
#
#  Read in the cereal dataset, create new variables 
#  and boxplots. Check some statistics. Examine if the 
#  average amount of sugar per serving varies by shelf height.  
# 
#  Change log:
#    2015-03-05 KY First Edition
#
#*******************************************************

# Assignment 7, Part 1 - Analysis of cereal data

# clear the workspace then set & show the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 7/Cereal")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(car)
library(ggplot2)
library(gridExtra)
library(plyr)
library(lsmeans)

# read in the data 
cereals <- read.csv("cereal.csv", as.is=TRUE, strip.white=TRUE, 
                    header=TRUE, fill=TRUE)

# replace -1 to missing value (NA)  
cereals[cereals==-1] <- NA
any(cereals==-1, na.rm=TRUE)

# print the first 10 records after fixedup
cereals[1:10,]



# index the list of character values by the value of shelf
ls()
c("low","medium","high")[cereals$shelf]

# Create a new variable shelfC
cereals$shelfC <- recode(cereals$shelf, "1='low'; 2='medium'; 3='high'")
xtabs(~shelf+shelfC, data=cereals)

# inspect the first few and last few records
head(cereals)
tail(cereals)

# check the attributes of the data
str(cereals)



# create a NEW variable shelfCF as the factor equivalent of shelfC
cereals$shelfCF <- factor(cereals$shelfC)

# print the first 10 records
cereals[1:10,]

# check the attributes of the data
str(cereals)



# reorder the levels of a factor
cereals$shelfCFO <- factor(cereals$shelfC, levels=c("low","medium","high"), ordered=TRUE)

# print the first 10 records
cereals[1:10,]

# check the attributes of the data
str(cereals)



# make box plots useing an ordered factor(shelfCFO)
plot.dot.box.CFO <- ggplot(data=cereals, aes(x=shelfCFO, y=sugars))+
  ggtitle("Sugars by shelf - using an ordered factor")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE, alpha=0.7)+
  geom_jitter(position=position_jitter(w=0.1))
plot.dot.box.CFO

# check the attributes of the box plot
str(plot.dot.box.CFO)

# save the plot
ggsave(plot=plot.dot.box.CFO, file='cereal-dot-plot.png',
       h=4, w=6, units="in", dpi=300)



# make box plots useing factor(shelfCF)
plot.dot.box.CF <- ggplot(data=cereals, aes(x=shelfCF, y=sugars))+
  ggtitle("Sugars by shelf - using shelfCF")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE, alpha=0.7)+
  geom_jitter(position=position_jitter(width=0.1, height=0.1))
plot.dot.box.CF

# make box plots useing character variable(shelfC)
plot.dot.box.C <- ggplot(data=cereals, aes(x=shelfC, y=sugars))+
  ggtitle("Sugars by shelf - using shelfC")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE, alpha=0.7)+
  geom_jitter(position=position_jitter(width=0.1, height=0.1))
plot.dot.box.C

# make box plots useing qualitative variable(shelf)
plot.dot.box <- ggplot(data=cereals, aes(x=shelf, y=sugars))+
  ggtitle("Sugars by shelf - using shelf")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE, alpha=0.7)+
  geom_jitter(position=position_jitter(width=0.1, height=0.1))
plot.dot.box

# put these 4 plots into one big plot
plot.dot.box.all <- arrangeGrob(plot.dot.box.CFO,
                                plot.dot.box.CF,
                                plot.dot.box.C,
                                plot.dot.box, ncol=2)
plot.dot.box.all



# some statistics about the amount of calories and sugar/serving
# the mean of the calories 
mean(cereals$calories)
with(cereals, mean(calories))

# the result of the mean of the sugar/serving WITH missing value 
with(cereals, mean(sugars))

# the mean of the sugar/serving WITHOUT missing value 
with(cereals, mean(sugars, na.rm=TRUE))

# the number of ALL values in the sugars variable
with(cereals, length(sugars))

# the number of NON-missing values in the sugars variable
with(cereals, length(na.omit(sugars)))
with(cereals, sum(!is.na(sugars)))

# the number of MISSING values in the sugars variable
with(cereals, sum(is.na(sugars)))



# compute some summary statistics by shelf
shelf.sum <- ddply(cereals, "shelf", summarize,
                   n.cereals = length(name),
                   nmiss.calories = sum(is.na(calories)),
                   mean.calories = mean(calories),
                   std.calories = sd(calories),
                   se.calories = std.calories/sqrt(length(na.omit(calories))),
                   lcl.calories=mean.calories-qt(0.975,length(na.omit(calories))-1)*se.calories,
                   ucl.calories=mean.calories+qt(0.975,length(na.omit(calories))-1)*se.calories,
                   nmiss.sugars =sum(is.na(sugars)),
                   mean.sugars = mean(sugars, na.rm=TRUE),
                   std.sugars = sd(sugars, na.rm=TRUE),
                   se.sugars = std.sugars/sqrt(length(na.omit(sugars))),
                   lcl.sugars=mean.sugars-qt(0.975,length(na.omit(sugars))-1)*se.sugars,
                   ucl.sugars=mean.sugars+qt(0.975,length(na.omit(sugars))-1)*se.sugars)
shelf.sum



# using function to compute some summary statistics
mysummary <- function(cereals, sugars, alpha=0.05){
  values <- cereals[, sugars]
  ntotal <- length(values)
  nonmiss <- length(na.omit(values))
  mean <-  mean(values, na.rm=TRUE)
  sd <- sd(values, na.rm=TRUE)
  se <- sd/sqrt(nonmiss)
  lcl <-  mean-qt(1-alpha/2,nonmiss-1)*se
  ucl <-  mean+qt(1-alpha/2,nonmiss-1)*se
  return(data.frame(ntotal, nonmiss, mean, sd, se, lcl, ucl))
}
mysummary(cereals, "sugars")



# using function to compute summary statistics by qualitative variable-shelf
summary.shelf <- ddply(cereals, "shelf", mysummary, sugars="sugars")
summary.shelf
str(summary.shelf)

# compute summary statistics by a character value-shelfC
summary.shelfC <- ddply(cereals, "shelfC", mysummary, sugars="sugars")
summary.shelfC

# compute summary statistics by an factor-shelfCF
summary.shelfCF <- ddply(cereals, "shelfCF", mysummary, sugars="sugars")
summary.shelfCF

# compute summary statistics by an ordered factor-shelfCFO
summary.shelfCFO <- ddply(cereals, "shelfCFO", mysummary, sugars="sugars")
summary.shelfCFO



# create a plot the mean by shelf with the 95% CI using shelf
mean.plot.shelf <- ggplot(data=summary.shelf, aes(x=shelf, y=mean))+
  ggtitle("Sugars by shelf - using a qualitative variable")+
  xlab("Shelf")+ylab("Mean sugar/serving(g) for each shelf")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)+
  geom_line(aes(group=1))
mean.plot.shelf

# create a plot the mean by shelf with the 95% CI using shelfC
mean.plot.shelfC <- ggplot(data=summary.shelfC, aes(x=shelfC, y=mean))+
  ggtitle("Sugars by shelf - using a character variable")+
  xlab("Shelf")+ylab("MEAN sugar/serving(g) & 95% CI")+  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)+
  geom_line(aes(group=1))
mean.plot.shelfC

# create a plot the mean by shelf with the 95% CI using shelfCF
mean.plot.shelfCF <- ggplot(data=summary.shelfCF, aes(x=shelfCF, y=mean))+
  ggtitle("Sugars by shelf - using a factor")+
  xlab("Shelf")+ylab("MEAN sugar/serving(g) & 95% CI")+  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)+
  geom_line(aes(group=1))
mean.plot.shelfCF

# create a plot the mean by shelf with the 95% CI using shelfCFO
mean.plot.shelfCFO <- ggplot(data=summary.shelfCFO, aes(x=shelfCFO, y=mean))+
  ggtitle("Sugars by shelf - using an ordered factor")+
  xlab("Shelf")+ylab("MEAN sugar/serving(g) & 95% CI")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)+
  geom_line(aes(group=1))
mean.plot.shelfCFO



# do formal hypothesis tests by  using shelf (NOT what we wanted)
my.fit.shelf <- lm(sugars ~ shelf, data=cereals)

# do formal hypothesis tests by  using shelfC
my.fit.shelfC <- lm(sugars ~ shelfC, data=cereals)

# do formal hypothesis tests by  using shelfCF
my.fit.shelfCF <- lm(sugars ~ shelfCF, data=cereals)

# do formal hypothesis tests by  using shelfCFO
my.fit.shelfCFO<- lm(sugars ~ shelfCFO, data=cereals)

# compare the outputs from ANOVA
anova(my.fit.shelf)
anova(my.fit.shelfC)
anova(my.fit.shelfCF)
anova(my.fit.shelfCFO)

# compare the outputs from summary
summary(my.fit.shelf)
summary(my.fit.shelfC)
summary(my.fit.shelfCF)
summary(my.fit.shelfCFO)


# produces the marginal means and 95% CI using a factor(shelfCF)
my.fit.shelfCF.lsmeans <- lsmeans::lsmeans(my.fit.shelfCF, ~shelfCF, adjust="tukey")


# produces the marginal means and 95% CI using an ordered factor(shelfCFO)
my.fit.shelfCFO.lsmeans <- lsmeans::lsmeans(my.fit.shelfCFO, ~shelfCFO, adjust="tukey")


# compare the outputs and check if they are identical
summary(my.fit.shelfCF.lsmeans)
summary(my.fit.shelfCFO.lsmeans)

cld (my.fit.shelfCF.lsmeans)
cld (my.fit.shelfCFO.lsmeans)

pairs (my.fit.shelfCF.lsmeans)
pairs (my.fit.shelfCFO.lsmeans)

confint(pairs(my.fit.shelfCF.lsmeans))
confint(pairs(my.fit.shelfCFO.lsmeans))



# save the output from the lsmeans calls
shelfCFO.lsmeans <- summary(my.fit.shelfCFO.lsmeans, digits=2)
shelfCFO.lsmeans
str(shelfCFO.lsmeans)

# make a plot of the means and 95% CI based on the lsmeans() output
plot.shelfCFO.means2 <- ggplot(data=shelfCFO.lsmeans, aes(x=shelfCFO, y=lsmean))+
  ggtitle("Sugars by shelf - using a ordered factor")+
  xlab("Shelf")+ylab("Estimated mean of sugar/serving along with 95% CI")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.2)+
  geom_line(aes(group=1))
plot.shelfCFO.means2

# Create the diagnostic plots
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
sf.autoplot.lm(my.fit.shelfCFO)



# save the table
sink('assign07-part01-cereal-table.txt', split=TRUE)
options(digits=3)
shelfCFO.lsmeans
sink()

# save the plot
ggsave(plot.shelfCFO.means2, file='assign07-part01-lsmean.png',
       h=4, w=6, units="in", dpi=200)




