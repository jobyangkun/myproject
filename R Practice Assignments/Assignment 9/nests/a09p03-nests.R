#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 9, Part 3  - Cigarette Butts and Nests
#
#  Read in the cigarette butts & nest data.Examine 
#  if there is the effect of Species and Nest Content 
#  on the MEAN number of parasites. Check the errors of
#  the figure 1.
#  
#  Change log:
#    2015-03-18 KY First Edition
#
#*******************************************************

# Assignment 09, Part 03 - Analysis of Cigarette Butts in Nests

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 9/nests")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(xlsx)
library(plyr)
library(ggplot2)
library(car)

# read in the data & replace -1 to missing value (NA)
ndf <- read.xlsx('nests.xls', sheetName="Correlational", header=TRUE, stringsAsFactors=FALSE)
ndf[ndf==-1] <- NA
any(ndf==-1, na.rm=TRUE)

# List the first 10 records after fixups
ndf[1:10,]


# examine the effect of Species and Nest Content on the MEAN number of parasites
# check the levels of 2 factors Species & Nest.content
levels(as.factor(ndf$Species))
levels(as.factor(ndf$Nest.content))


# comparing the number of nests measured by host species and nest content
xtabs(~Species+Nest.content, data=ndf)


# compute mean and 95% CI for mean of mites
report <- ddply(ndf, .(Species,Nest.content), summarize,
                mean = mean(Number.of.mites, na.rm=TRUE),
                lcl  = mean-qt(0.975,length(Number.of.mites)-1)*
                       sd(Number.of.mites)/sqrt(length(Number.of.mites)),
                ucl  = mean+qt(0.975,length(Number.of.mites)-1)*
                       sd(Number.of.mites)/sqrt(length(Number.of.mites)))
report


# create the interaction plot using ggplot
plotInt <- ggplot(data=report, aes(x=Nest.content, y=mean,
                                   group=Species, color=Species, linetype=Species))+
  ggtitle("The average number of number of mites/nest")+
  ylab("Number of mites")+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2, position="dodge")
plotInt


# create two factors
ndf$SpeciesF <- factor(ndf$Species)
ndf$Nest.contentF <- factor(ndf$Nest.content)


#-------------------------------lm()-------------------------------------------------------------------
# set regression to examine the effect of Species and Nest Content on the MEAN number of parasites
my.lm.model1 <- lm(Number.of.mites ~ SpeciesF + Nest.contentF + SpeciesF:Nest.contentF, data=ndf)
my.lm.model3 <- lm(Number.of.mites ~ Nest.contentF + SpeciesF + SpeciesF:Nest.contentF, data=ndf)
my.lm.model2 <- lm(Number.of.mites ~ SpeciesF:Nest.contentF + Nest.contentF + SpeciesF, data=ndf)

# check the results
summary(my.lm.model1)
summary(my.lm.model2)
summary(my.lm.model3)

# display the F tests for the three models
anova(my.lm.model1)
anova(my.lm.model2)
anova(my.lm.model3)


#-----------------------Type III tests----------------------------------------------------
# Use the Type III tests from the Anova() function
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
options()$contrasts

# Check the Type III tests results
Anova(my.lm.model1, type=3)
Anova(my.lm.model2, type=3)
Anova(my.lm.model3, type=3)



#---------------------check why do not match figure 1-----------------------------
# ANCOVA with Butts.weight
my.lm.model4 <- lm(Number.of.mites ~ SpeciesF+Nest.contentF+
                     SpeciesF:Nest.contentF+Butts.weight, data=ndf)
Anova(my.lm.model4, type=3)


# Create a vector that takes the value of TRUE for the first 42 cases
ndf$newV <- "False"
ndf$newV[ndf$Nest %in% c(1:42)] <- "True"
ndf[1:10,]

# new regression based on the subset database
my.lm.model5 <- lm(Number.of.mites ~ SpeciesF+Nest.contentF+
                     SpeciesF:Nest.contentF+Butts.weight, data=subset(ndf,newV=="True"))
Anova(my.lm.model5, type=3)

# Create the diagnostic plots from the last analysis
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
sf.autoplot.lm(my.lm.model5)



# repeat the analysis based on ALL nests but using log(number of mites)
my.lm.model6 <- lm(log(Number.of.mites) ~ SpeciesF+Nest.contentF+
                     SpeciesF:Nest.contentF+Butts.weight, data=ndf)
Anova(my.lm.model6, type=3)
sf.autoplot.lm(my.lm.model6)



# create new vector for the two outliers
ndf$residual <- my.lm.model6$residual
ndf$newV2 <- "True"
ndf$newV2[ndf$residual %in% c(min(ndf$residual),max(ndf$residual))] <- "False"
ndf


#-----------------------final corrected regression------------------------------------
# new regression based on the subset database
final <- lm(log(Number.of.mites) ~ SpeciesF+Nest.contentF+
                     SpeciesF:Nest.contentF+Butts.weight, data=subset(ndf,newV2=="True"))
result <- Anova(final, type=3)
result
sf.autoplot.lm(final)
options(old.options)


# save the table
sink('table1.txt', split=TRUE)
result
sink()

