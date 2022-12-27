#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 7, Part 2  - Cereal
#
#  Read in the accidents data, test hypotheses.Examine 
#  if the distribution of accidents varies by months or hours.
#  Create the histograms of number of accidents by the minute or the hour
#  
#  Change log:
#    2015-02-25 KY First Edition
#
#*******************************************************

# Assignment 7, Part 2 - Analysis of accidents data

# clear the workspace then set & show the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 7/Accident")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(plyr)
library(ggplot2)
library(lsmeans)

# read in the data 
radf <- read.csv("road-accidents-2010.csv", as.is=TRUE, strip.white=TRUE, 
                    header=TRUE, fill=TRUE)

# replace -1 to missing value (NA)  
radf[radf==-1] <- NA

# print the first 10 records after fixedup
radf[1:10,]


# convert the character variable to the number of days
radf$myDate <- as.Date(radf$Date, "%d/%m/%Y")
sum(is.na(radf$myDate))
radf$myDate[1:10]

# count the number of accidents in each date
naccidents <- ddply(radf, "myDate", summarize,
                   n.accidents = length(myDate))
naccidents[1:10,]

# create a scatter plot of the number of accidents in each date
plot.date <- ggplot(data=naccidents, aes(x=myDate, y=n.accidents))+
  ggtitle("scatter plot of the number of accidents")+
  xlab("Number of accidents")+ylab("Date")+
  geom_point(size=1)+
  geom_smooth()
plot.date

# Extract the month
naccidents$month <- as.numeric(format(naccidents$myDate, "%m"))
naccidents[1:10,]

# reorder the levels of a factor
naccidents$monthF <- factor(naccidents$month)


# A summary of the number of days, the mean accidents/day, 
# the SD of the accidents per day for each month,
# the se for the mean and a 95% CI
mysummary <- function(naccidents, n.accidents, alpha=0.05){
  values <- naccidents[, n.accidents]
  nday <- length(na.omit(values))
  mean <-  mean(values, na.rm=TRUE)
  sd <- sd(values, na.rm=TRUE)
  se <- sd/sqrt(nday)
  lcl <-  mean-qt(1-alpha/2,nday-1)*se
  ucl <-  mean+qt(1-alpha/2,nday-1)*se
  return(data.frame(nday, mean, sd, se, lcl, ucl))
}
summary <- ddply(naccidents, "monthF", mysummary, n.accidents="n.accidents")
summary


# create a plot the mean number of accidents/day by month with the 95% CI
mean.plot.month <- ggplot(data=summary, aes(x=monthF, y=mean))+
  ggtitle("Number of accidents/day by month")+
  xlab("month")+ylab("Mean number of accidents/day")+
  scale_x_discrete(lim=1:12)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)
mean.plot.month

# save the plot
ggsave(mean.plot.month, file='assign07-part02-mean.png',
       h=4, w=6, units="in", dpi=200)


# do formal lm hypothesis tests
my.fit <- lm(n.accidents ~ monthF, data=naccidents)
anova(my.fit)
summary(my.fit)


# produces the lsmean and 95% CI
my.fit.lsmeans <- lsmeans::lsmeans(my.fit, ~monthF, adjust="tukey")
summary(my.fit.lsmeans)
cld (my.fit.lsmeans)


# Dealing time-stamp date, convert to numerical data
radf$Time[ radf$Time==''] <- "12:00"
radf$DateTime <- as.POSIXct(paste(radf$Date," ",radf$Time),
                            format="%d/%m/%Y %H:%M", tz="UTC")
radf$DateTime[1:10]
str(radf$DateTime)
as.numeric(radf$DateTime[1:10])


# extracts the numeric hours for each date-time
radf$hour <- as.numeric(format(radf$DateTime, "%H"))
radf[1:10,c("DateTime","hour")]

# extracts the numeric minutes for each date-time
radf$min <- as.numeric(format(radf$DateTime, "%M"))
radf[1:10,c("DateTime","min")]

# create a a histogram of Number of accidents by hour of the day
histogram <- ggplot(data=radf, aes(x=hour))+
  ggtitle("Number of accidents per hour")+
  xlab("hour")+ylab("Number of accidents")+
  geom_histogram(binwidth=1, alpha=0.7)
histogram

# create a a histogram of Number of accidents by the minute of the hour
histogram2 <- ggplot(data=radf, aes(x=min))+
  ggtitle("Number of accidents per minute")+
  xlab("Minute")+ylab("Number of accidents")+
  geom_histogram(binwidth=1, alpha=0.7)
histogram2

# save the histogram
ggsave(histogram2, file='assign07-part02-histogram.png',
       h=4, w=6, units="in", dpi=200)

