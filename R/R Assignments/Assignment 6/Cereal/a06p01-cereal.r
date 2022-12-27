#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 6, Part 1  - Cereal
#
#  Read in the cereal dataset, do casement & scatter plots, 
#  then do a regression of calores against fat. 
#  finally,plot residuals and get predictions at 4 gram fat.
#  
#  Change log:
#    2015-02-25 KY First Edition
#
#*******************************************************

# Assignment 6, Part 1 - Analysis of cereal data

# clear the workspace then set & show the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 6/Cereal")
cat("Current directory is", getwd(), "\n")

# load required libraries.
library(ggplot2)
library(GGally)

# read in the data and check the stucture of the object.
cereals <- read.csv("cereal.csv", as.is=TRUE, strip.white=TRUE, header=TRUE, fill=TRUE)
str(cereals)

# verify that the data was read in properly.
names(cereals)
dim(cereals)
head(cereals)
tail(cereals)

# print the first 10 records of the cereals data frame.
cereals[1:10,]



# Check for outliers and missing values.
# replace -1 to missing value (NA) in potass column. 
cereals$potass
cereals$potass==-1
cereals$potass[cereals$potass==-1] <- NA

# replace -1 to missing value (NA) in the entire data frame. 
any(cereals==-1, na.rm=TRUE)
cereals[cereals==-1] <- NA
any(cereals==-1, na.rm=TRUE)

# print the first 10 records after fixed up all of the potential missing values.
cereals[1:10,]



# create a casement plot of variables from cereals dataset. 
scatmat <- ggpairs(cereals[,c("protein","fat","carbo","sugars","sodium","vitamins")],
   title="Casement plot of variables from cereals dataset")
scatmat

# create a scatter plot of calories vs grams of fat.
plotcalfat <- ggplot(data=cereals,aes(x=fat,y=calories))+
   ggtitle("Calories vs fat content")+
   geom_point()
plotcalfat

# jitter the points for plotting.
plotcalfat2 <- ggplot(data=cereals,aes(x=fat,y=calories))+
  ggtitle("Calories vs fat content - point jittered")+
  geom_jitter()
plotcalfat2



# Do a regression of calories against grams of fat.
my.fit <- lm(calories ~ fat, data=cereals)

# get the ANOVA table and hypothesis testing about effects.
anova(my.fit)

# get the table of coefficients.
summary(my.fit)



# Extract the individual coefficients.
my.fit.coef <- coef(my.fit) 
my.fit.coef

# the standard errors of the coefficients.
vcov(my.fit)
my.fit.se <- sqrt(diag(vcov(my.fit))) 
my.fit.se

# the confidence intervals for the slope and intercept.
my.fit.ci <- confint(my.fit)
my.fit.ci

# put them together into a little table.
my.table <- data.frame(Coef=round(my.fit.coef,2),
                       SE=round(my.fit.se,2),
                       CI=round(my.fit.ci,2))
my.table

# save TEXTUAL output to a file.
sink('assign06-part01-cereal-table1.txt', split=TRUE)
my.table
sink()



# add the fitted line to the previous plot.
plotcalfat3 <- plotcalfat2+geom_abline(intercept=my.fit.coef[1], slope=my.fit.coef[2])
plotcalfat3
ggsave(plotcalfat3,file='assign06-part01-calfat.png',h=4,w=6,units="in",dpi=200)

# Create the model diagnostic plot.
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
diagplot <- sf.autoplot.lm(my.fit)
diagplot



# get new predictions at 4 gram fat.
# construct a data.frame with the same predictor variable.
newfat <- data.frame(fat=c(4))
newfat

# get the predictions at 4 gram fat.
my.fit.at4.mean <- predict(my.fit, newdata=newfat, se.fit=TRUE, interval="confidence")
my.fit.at4.mean
my.fit.at4.indiv <- predict(my.fit, newdata=newfat, se.fit=TRUE, interval="prediction")
my.fit.at4.indiv

