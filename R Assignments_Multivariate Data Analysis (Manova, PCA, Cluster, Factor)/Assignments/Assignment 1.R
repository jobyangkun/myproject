### Read the file on major league baseball statistics into R
setwd("C:\\Users\\Kun\\Desktop\\homework")
getwd()
MLB<-read.csv("Major League Baseball Main Stats Altered 2014.csv")
MLB


###mean vector 
(round(colMeans(MLB[,2:6]),3))

###variance-covariance matrix 
(round(cov(MLB[,2:6]),3))

###correlation matrix
(round(cor(MLB[,2:6]),3))


###all pairs of two-variable scatterplots
pairs(MLB[2:6])

###function panel.cor
panel.cor <- function(x, y, digits=3, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  ###hypothesis testing with null hypothesis: "true correlation is equal to zero"
  test <- cor.test(x,y) 
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  ###correlation coefficients
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

pairs(MLB[2:6],lower.panel=panel.cor)



###the univariate boxplots.
boxplot(MLB[,2], pch=19, main="boxplot", ylab="Mean Salary")
boxplot(MLB[,3], main="boxplot", ylab="Percent Wins")
boxplot(MLB[,4], main="boxplot", ylab="Batting Average")
boxplot(MLB[,5], main="boxplot", ylab="Earned Run Average")
boxplot(MLB[,6], main="boxplot", ylab="Errors")



###Constructing the bivariate boxplot.
library("MVA")
X<-MLB[5]
Y<-MLB[3]
bvbox(cbind(X, Y),7, pch=20,main="Bivariate Boxplot", xlab="Earned Run Average",ylab="Percent Wins")

