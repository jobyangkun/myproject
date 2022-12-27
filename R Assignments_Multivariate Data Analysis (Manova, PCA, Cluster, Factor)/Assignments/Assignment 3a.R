setwd("C:\\Users\\Kun\\Desktop\\homework")

data<-read.csv("Assignment 3a Data.csv",header=0)
data


###the univariate boxplots.
boxplot(data, pch=20, main="Univariate Boxplots")

###Bivariate boxplots
library("MVA")
X<-MLB[1]
Y<-MLB[2]
bvbox(cbind(X, Y),7, pch=20,main="Bivariate Boxplot")


library(scatterplot3d)
scatterplot3d(data,pch=20,main="3D Scatterplot")



plot(data,type="l")



