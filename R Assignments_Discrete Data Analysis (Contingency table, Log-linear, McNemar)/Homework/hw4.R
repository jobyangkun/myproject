temp<-c(66, 70, 69, 68, 67, 72, 73, 70, 57, 63, 70, 78, 67, 53, 67, 75, 70, 81, 
          76, 79, 75, 76, 58)
TD<-c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1)
table<-data.frame(temp, TD)
mod.fit<-glm(formula=TD~temp, data=table, family=binomial(link=logit))
summary(mod.fit)

