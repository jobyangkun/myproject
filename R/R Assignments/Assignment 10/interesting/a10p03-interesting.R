#*******************************************************
#
#  Kun, Yang   301178299
#  Stat 340, Spring 2015
#  Assignment 10, Part 3  - interesting
#
#  create interesting residual plots.
#  
#  Change log:
#    2015-03-24 KY First Edition
#
#*******************************************************

# Assignment 10, Part 03 - interesting

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/340/R/Assignment 10/interesting")
cat("Current directory is", getwd(), "\n")

# load required libraries
library(ggplot2)
library(GGally)


# Read in the data
data <- read.csv('interesting2.csv', as.is=TRUE, fill=TRUE,
                    strip.white=TRUE, header=TRUE)
data[1:10,]


# Create a scatterplot matrix of all variables.
scatmat <- ggpairs(data, title="scatterplot matrix")
scatmat


# Multiple Linear Regression
fit <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data=data)
summary(fit)


# Extract the residuals and predicted values
pred <- fitted(fit) # predicted values
res <- residuals(fit) # residuals
plot.data <- data.frame(pred,res)
plot.data [1:10,]


# Plot the residual against the predicted value
plot <- ggplot(data=plot.data, aes(x=pred, y=res))+
  ggtitle("Residual plot")+
  xlab("predicted value")+ylab("residual")+
  geom_point()+
  scale_x_continuous(limits = c(-2,2))
plot

ggsave(plot=plot, file="plot.png",h=4, w=6, units='in', dpi=300)





