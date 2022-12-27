rm(list=ls())
setwd("C:\\Users\\Kun\\Desktop\\homework")
data<-read.csv("445 Project.csv")
data
summary(data[2:9])
sd(data[,2])
sd(data[,3])
sd(data[,4])
sd(data[,5])
sd(data[,6])
sd(data[,7])
sd(data[,8])
sd(data[,9])

###Principal components analysis
###The eigenvalues and eigenvectors:
library("MVA")
(COV=cov(data[,2:9]))
eig=eigen(COV)
colnames(eig$vectors)=c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')
rownames(eig$vectors)=c('CF','HF','RER','AT','HP','TR','SF','FO')
eig
###Then the rescaled eigenvalues:
(eig$values/sum(eig$values))

plot(1:length(eig$values),eig$values, main="Scree Plot",
     xlab="Principal Component Number", ylab="Principal Component Variance",type="b")



###factor analysis (analysis with 4 factors)
(FA=factanal(covmat=COV,factors=4,n.obs=80) )
L=FA$loadings
load<-factanal(~data[,2]+data[,3]+data[,4]+data[,5]+data[,6]+data[,7]+data[,8]+data[,9],
         factors=4,scores ="Bartlett")$scores
rownames(load)=data[,1]
load
x<-load[,1]
y<-load[,3]
plot(x,y,main="Scatter Plot",xlab="Score for Factor 1",ylab="Score for Factor 3",cex=1.5,pch=18)
text(x,y,row.names(load), cex=1, pos=4, col="red")


(FA=factanal(covmat=COV,factors=4,n.obs=80,rotation="varimax") )

###communalities
(hi2=(L%*%t(L)))

###specific variances
(psi=diag(FA$uniquenesses))

###residual matrix and round it to 4 decimal places
RezMat=COV-(hi2+psi)
round(RezMat,4)
###diagonal elements of the residual matrix are 0 which means that (hi2+psi) is close to the correlation matrix

###Part C
(A<-det(hi2+psi))
(B<-det(CORR))
(C<-50-1-(2*7+4*3+5)/6)
(Test<-C*log(A/B))

FA$Factor1

A=L$Factor1
B=L$Factor2
plot(duration, waiting,            # plot the variables 
     +   xlab="Eruption duration",        # x???axis label 
     +   ylab="Time waited") 

