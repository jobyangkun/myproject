rm(list=ls())
(CORR<- matrix(c(1,0.4919,0.2636,0.4653,-0.2277,0.0652,
                0.4919,1,0.3127,0.3506,-0.1917,0.2045,
                0.2635,0.3127,1,0.4108, 0.0647,0.2493,
                0.4653,0.3506,0.4108,1,-0.2249,0.2293,
                -0.2277,-0.1917,0.0647,-0.2249,1,-0.2144,
                0.0652,0.2045,0.2493,0.2293,-0.2144,1),ncol=6,byrow=TRUE))

(eig=eigen(CORR))
###analysis with 3 factors
(FA=factanal(covmat=CORR,factors=3) )
str(FA)
L=FA$loadings

###communalities
(hi2=(L%*%t(L)))

###specific variances
(psi=diag(FA$uniquenesses))

###residual matrix and round it to 4 decimal places
RezMat=CORR-(hi2+psi)
round(RezMat,4)
###diagonal elements of the residual matrix are 0 which means that (hi2+psi) is close to the correlation matrix

###analysis with 2 factors
(FA=factanal(covmat=CORR,factors=2) )


###Problem 9.19
###Part (a) Obtain only the maximum likelihood estimates for the factor analysis model with m = 2 and m = 3.
rm(list=ls())
Data.matrix<-read.csv("Salespeopledata.csv")
CORR=cor(Data.matrix)
(eig=eigen(CORR))


###analysis with 3 factors
(FA=factanal(covmat=CORR,factors=3) )
str(FA)
L=FA$loadings

###communalities
(hi2=(L%*%t(L)))

###specific variances
(psi=diag(FA$uniquenesses))

###residual matrix and round it to 4 decimal places
RezMat=CORR-(hi2+psi)
round(RezMat,4)
###diagonal elements of the residual matrix are 0 which means that (hi2+psi) is close to the correlation matrix

###Part C
(A<-det(hi2+psi))
(B<-det(CORR))
(C<-50-1-(2*7+4*3+5)/6)
(Test<-C*log(A/B))

###analysis with 2 factors
(FA=factanal(covmat=CORR,factors=2) )
str(FA)
L=FA$loadings

###communalities
(hi2=(L%*%t(L)))

###specific variances
(psi=diag(FA$uniquenesses))

###residual matrix and round it to 4 decimal places
RezMat=CORR-(hi2+psi)
round(RezMat,4)
###diagonal elements of the residual matrix are 0 which means that (hi2+psi) is close to the correlation matrix

###Part C
(A<-det(hi2+psi))
(B<-det(CORR))
(C<-50-1-(2*7+4*2+5)/6)
(Test<-C*log(A/B))




