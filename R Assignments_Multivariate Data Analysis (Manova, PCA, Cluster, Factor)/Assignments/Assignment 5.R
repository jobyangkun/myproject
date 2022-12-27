rm(list=ls())

### 1.Use the scan () function to enter these data as a matrix.
Data.matrix <- matrix(scan(n=24),nrow=6, byrow=TRUE)
56.8000  -124.9000  680  1129
55.5333  -122.4833  680  1231
53.3667  -120.2500  771  1342
51.2333  -116.6667  1143  1150
57.0000  -122.3667  1204  842
51.3000  -116.9667  787  1514

### 2.Label both the rows and columns of your matrix with meaningful, short labels.
colnames(Data.matrix)=c('Lat','Long','Elev','GDD')
rownames(Data.matrix)=c('IP','PP','MN','YP','PM','GO')
Data.matrix

### 3.Compute the correlation matrix for these data. Which variable pairs, if any, are now highly correlated?
round(cor(Data.matrix),4)
### Latitude and Longitude are highly correlated.
  
### 4.What do the entries in the correlation matrix suggest about which of these variables might be related to GDD?
GDD = Data.matrix[,4]
Lat = Data.matrix[,1]
Long = Data.matrix[,2]
Elev = Data.matrix[,3]

full.model.fit = lm(GDD ~ Lat + Long + Elev)
summary(full.model.fit)
drop1(full.model.fit)

### Drop Elevation.
full.model.fit1 = lm(GDD ~ Lat + Long)
summary(full.model.fit1)
drop1(full.model.fit1)


### 5.Evaluate the eigenvalues and eigenvectors of the correlation matrix for 
### the x-variables,latitude, longitude, and elevation.
eigens = eigen(cor(Data.matrix[,c(1,2,3)]))
(evals = eigens$values)
(evecs = eigens$vectors)

### 6.Evaluate the sum of these eigenvalues. You should have found this sum to be an integer. Why
### does the sum equal this particular integer?
sum(evals)

7. Show that one of the standard guidelines (pick any one that you like) suggests that you can
ignore the minor dispersion in the direction of the smallest eigenvalue.
8. Describe, with reference to the components of its eigenvector, the direction of variation
associated with this eigenvalue.
9. Describe, with reference to the map of the sites that is reproduced below, the reason why
there is so little variation in this direction.

### 10. Compute a matrix of standardized values for the three variables, latitude, longitude, and elevation.
Lat.std =(Lat-mean(Lat))/sd(Lat)
Long.std =(Long-mean(Long))/sd(Long)
Elev.std =(Elev-mean(Elev))/sd(Elev)

StdMatrix<-cbind(Lat.std,Long.std,Elev.std)
StdMatrix

### 11.The variance-covariance for this matrix should match something you have already calculated.
round(cov(StdMatrix),4)


### 12.a. For the dimensions in the product to be compatible, which matrix should appear first in the product?
### b. Should you use the matrix or eigenvectors or its transpose?
PC = StdMatrix %*% evecs
eigen(cov(PC))

### 13. Do a linear regression of GDD on all three principal component vectors. 
### One of the principal components appears not to be useful in helping to predict GDD. 
### Which one is this? To which direction does this correspond?

PC1=PC[,1]
PC2=PC[,2]
PC3=PC[,3]
PC.reg <- lm(GDD ~ PC1+PC2+PC3)
summary(PC.reg)

PC.reg1 <- lm(GDD ~ PC1+PC2)
summary(PC.reg1)



