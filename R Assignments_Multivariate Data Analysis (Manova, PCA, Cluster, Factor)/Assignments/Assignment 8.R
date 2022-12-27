rm(list=ls())
dist_mat<-as.matrix(read.csv("T12-13.csv",header=F))
dist_mat[upper.tri(dist_mat)]=as.integer(0)
dist_mat=dist_mat+t(dist_mat)
rownames(dist_mat)=c('P1980918','P1931131','P1550960','P1530987','P1361024','P1351005','P1340945','P1311137','P1301062')
colnames(dist_mat)=c('P1980918','P1931131','P1550960','P1530987','P1361024','P1351005','P1340945','P1311137','P1301062')
dist_mat

###  Multidimensional scaling  with 3 dimensions
library(MASS)
(nMDS3=isoMDS(d=dist_mat,k = 3))

###  Multidimensional scaling  with 4 dimensions
(nMDS4=isoMDS(d=dist_mat,k = 4))

###  Multidimensional scaling  with 5 dimensions
(nMDS5=isoMDS(d=dist_mat,k = 5))

x=nMDS5$points[,1]
y=nMDS5$points[,2]

plot(x,y, xlab="Coordinate 1", ylab="Coordinate 2", main="Nonmetric  MDS")
text(x, y, labels = rownames(dist_mat), cex=.9,col=rainbow(10))

#plot of the stress function against the MDS dimension
stress_vector=rep(NA,7)
stress_vector[2]=nMDS5$stress
for (i in (3:7)){ stress_vector[i]=isoMDS(d=dist_mat,k = i)$stress}
plot(stress_vector,type="l",xlab="q",ylab="Stress",main="Stress function")

