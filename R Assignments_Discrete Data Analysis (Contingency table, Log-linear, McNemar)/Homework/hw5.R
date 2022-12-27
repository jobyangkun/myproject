rm(list=ls())
setwd("C:\\Users\\Kun\\Desktop\\homework")
data1<-read.csv("Table.csv",header=TRUE,sep=",")

data1.lm <-glm(cbind(Yes,No)~EI+SN+TF+JP,family=binomial, data=data1)
summary(data1.lm)
