rm(list=ls())
setwd("C:\\Users\\Kun\\Desktop\\homework")
data<-read.csv("Assignment 7 Cereal Data.csv")
data
###distance method: euclidian
(mat<- dist(data[4:11],method="euclidian"))

hc_c=hclust(mat, method='centroid')
plot(hc_c,hang=-1,labels=data[,2])

hc_c=hclust(mat, method="ward")
plot(hc_c,hang=-1,labels=data[,2])


###K-Means Cluster Analysis
# 3 cluster solution
(fit3<-kmeans(data[4:11], 3))
st <- sort(fit3$cluster)
split(st, st)


# 4 cluster solution
(fit4<-kmeans(data[4:11], 4))
st <- sort(fit4$cluster)
split(st, st)

# 5 cluster solution
(fit5<-kmeans(data[4:11], 5))
st <- sort(fit5$cluster)
split(st, st)


wss <- (nrow(data[4:11])-1)*sum(apply(data[4:11],2,var)) 
for (i in 2:43) wss[i] <- sum(kmeans(data[4:11],centers=i)$withinss)
plot(1:43, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
wss
