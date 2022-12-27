 # Assignment 7, Part 2   2015-02-06  
 
#*****************************************************
#                                                    *
#  Schwarz, Carl   123456789                         *
#  Stat 340, Spring 2015                             *
#  Assignment 7, Part 2  
#  Look at distribution of accidents in various ways *
#                                                    *
#*****************************************************

# Verify the working directory
getwd()

# Clear the workspace of detrius
rm(list=ls())

# load the necessary packages
library(car)
library(ggplot2)
library(lsmeans)
library(plyr)

# Read in the data, look at the attributes of the data
# and display the first few records
radf <- read.csv('../Accidents/road-accidents-2010.csv', as.is=TRUE, header=TRUE)
str(radf)
dim(radf)
head(radf)


# Convert the dates to R date values
radf$myDate <- as.Date(radf$Date, "%d/%m/%Y")
sum(is.na(radf$myDate))
radf$myDate[1:10]


# Count the number of accidents by date
library(plyr)
naccidents <- ddply(radf, "myDate", summarize, 
                N.accidents=length(myDate))
naccidents[1:10,]
str(naccidents)
sum(naccidents$N.accidents)
dim(radf)

plot.naccidents <- ggplot(data=naccidents, aes(x=myDate, y=N.accidents))+
    ggtitle("Number of accidents with injury in 2010")+
    xlab("Date")+ylab("Number of accidents")+
    geom_point()+
    geom_smooth()
plot.naccidents


# Extract the month
naccidents$month <- as.numeric(format(naccidents$myDate, "%m"))
naccidents[1:10,]



# Lets write a small summary function
mysummary <- function(mydf, myvar){
# Compute the number of element, number of non-missing elements
# mean, sd, and se of mydf$myvar
# mydf is assumed to be a dataframe
  x       <- mydf[,myvar]
  ntotal  <- length(x)
  nonmiss <- length(na.omit(x))
  mean    <- mean(x,na.rm=TRUE)
  sd      <- sd(x, na.rm=TRUE)
  se      <- sd(x, na.rm=TRUE)/sqrt(nonmiss)
  lcl     <- mean - qt(0.975,nonmiss-1)*se
  ucl     <- mean + qt(0.975,nonmiss-1)*se
  return(c(ntotal=ntotal,nonmiss=nonmiss,mean=mean,sd=sd,se=se,lcl=lcl,ucl=ucl))
}

# Find the summary for each month
summary.month <- ddply(naccidents, "month", mysummary, "N.accidents")
summary.month


plot.monthly.comp <- ggplot(data=summary.month, aes(x=month, y=mean))+
    ggtitle("Number of accidents by month")+
    xlab("Month")+ylab("Mean number of accidents by month with 95% ci")+
    geom_point()+
    geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)+
    geom_line(aes(group=1))+
    scale_x_discrete(lim=1:12)
plot.monthly.comp
ggsave(plot=plot.monthly.comp, file='accidents-monthly.png',
    h=4, w=6, units="in", dpi=300)


# Do a formal hypothesis test. Don't forget to make month factor.
# Make a month factor.
naccidents$monthF <- as.factor(naccidents$month)
my.month.lm <- lm(N.accidents ~ monthF, data=naccidents)
anova(my.month.lm)
summary(my.month.lm)

# look for difference in the means across the various  months
library(lsmeans)
month.lsmo <- lsmeans(my.month.lm, ~ monthF, adjust="tukey")
summary(month.lsmo)
cld    (month.lsmo)



#------------------------------------------------------------------------
# Now for the hour and minute data


# convert the Date and Time to dates and times
radf$Time[ radf$Time==''] <- "12:00"

radf$DateTime <- as.POSIXct(paste(radf$Date," ",radf$Time), format="%d/%m/%Y %H:%M", tz="UTC")
sum(is.na(radf$DateTime))
radf[ is.na(radf$DateTime), c("Date","Time","myDate", "DateTime")]


radf$DateTime[1:10]
str(radf$DateTime)
as.numeric(radf$DateTime[1:10])

radf$Date <- NULL  # drop these two variables
radf$Time <- NULL
str(radf)

# Extract the month
radf$month <- as.numeric(format(radf$DateTime, "%m"))
radf[1:10,c("DateTime","month")]
str(radf$month)



radf$hour <- as.numeric(format(radf$DateTime, "%H"))
radf$min  <- as.numeric(format(radf$DateTime, "%M"))
str(radf$hour)
str(radf$min)

plot.hour <- ggplot(data=radf, aes(x=hour))+
  ggtitle("Histogram of accidents with injury by hour of the day")+
  ylab("Number of accidents")+xlab("Hour of the day")+
  geom_histogram( binwidth=1, alpha=0.2)
plot.hour

plot.minute <- ggplot(data=radf, aes(x=min))+
  ggtitle("Histogram of accidents with injury by minute of the hour")+
  ylab("Number of accidents")+xlab("Minute of the hour")+
  geom_histogram( binwidth=1, alpha=0.2)
plot.minute
ggsave(plot=plot.minute, file="accidents-by-minute.png",
       h=4, w=6, units="in", dpi=300)
