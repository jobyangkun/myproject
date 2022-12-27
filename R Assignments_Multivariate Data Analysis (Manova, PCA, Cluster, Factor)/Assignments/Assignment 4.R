###Problem 1
setwd("C:\\Users\\Kun\\Desktop\\homework")
data<-read.csv("Assignment 3a Data.csv",header=0)
data

library("MVA")
cov.data<-cov(data)
data_PCA=eigen(cov.data)
data_PCA

data2<-transform(data[0],newColumn=-0.02770348*data[1]
                 -0.69461005*data[,2]+0.71885283*data[,3])
data2
boxplot(data2, pch=20, main="Boxplots")


###Problem 8.19
RFF<-read.csv("RecordsForFemales.csv")
RFF

RFF<-transform(RFF[1],newColumn=100/RFF[2],newColumn=200/RFF[3],newColumn=400/RFF[4],
                  newColumn=800/(RFF[5]*60),newColumn=1500/(RFF[6]*60),
                  newColumn=3000/(RFF[7]*60),newColumn=42195/(RFF[8]*60) ) 

RFF2<-RFF[,2:8]
colnames(RFF2)=c('100m/s','200m/s','400m/s','800m/s','1500m/s','3000m/s','Marm/s')
RFF2

library("MVA")
cov.RFF<-cov(RFF2)
cov.RFF

RFF_PCA=eigen(cov.RFF)
colnames(RFF_PCA$vectors)=c('PC1','PC2','PC3','PC4','PC5','PC6','PC7')
rownames(RFF_PCA$vectors)=c('100m/s','200m/s','400m/s','800m/s','1500m/s','3000m/s','Marm/s')
RFF_PCA

PC.RFF.variance<-eigen(cov.RFF)$values
PC.RFF.variables<-eigen(cov.RFF)$vectors
PC.RFF.var.prop<-PC.RFF.variance/sum(PC.RFF.variance)
PC.RFF.var.prop

RANKF<-transform(RFF[1],newColumn=0.3102442*RFF[2]
                 +0.3573948*RFF[3]+0.3787367*RFF[4]
                 +0.2993405*RFF[5]+0.3912131*RFF[6]
                 +0.4595909*RFF[7]+0.4227291*RFF[8])
RANKF[order(RANKF[2],decreasing=T),]

###Problem 8.20
RFM<-read.csv("Assignment 4a Data.csv")
RFM

RFM<-transform(RFM[1],newColumn=100/RFM[2],newColumn=200/RFM[3],
               newColumn=400/RFM[4],newColumn=800/(RFM[5]*60),
               newColumn=1500/(RFM[6]*60),newColumn=5000/(RFM[7]*60),
               newColumn=10000/(RFM[8]*60),newColumn=42195/(RFM[9]*60) ) 
RFM2<-RFM[,2:9]
colnames(RFM2)=c('100m/s','200m/s','400m/s','800m/s','1500m/s','5000m/s','10000m/s','Marm/s')
RFM2

library("MVA")
cov.RFM<-cov(RFM2)
cov.RFM

RFM_PCA=eigen(cov.RFM)
colnames(RFM_PCA$vectors)=c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')
rownames(RFM_PCA$vectors)=c('100m/s','200m/s','400m/s','800m/s','1500m/s','5000m/s','10000m/s','Marm/s')
RFM_PCA

PC.RFM.variance<-eigen(cov.RFM)$values
PC.RFM.variables<-eigen(cov.RFM)$vectors
PC.RFM.var.prop<-PC.RFM.variance/sum(PC.RFM.variance)
PC.RFM.var.prop

RANKF<-transform(RFF[1],newColumn=0.3102442*RFF[2]
                 +0.3573948*RFF[3]+0.3787367*RFF[4]
                 +0.2993405*RFF[5]+0.3912131*RFF[6]
                 +0.4595909*RFF[7]+0.4227291*RFF[8])
RANKF[order(RANKF[2],decreasing=T),]

