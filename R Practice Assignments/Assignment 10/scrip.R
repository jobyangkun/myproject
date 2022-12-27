logit<- function(lambda,data,negll=FALSE){
  res<- sum(dpois(data,lambda,log=TRUE))
  if(negll)(res<-res)
  return(res)
}

mydata<-c(4, 9,14)
logit

sd(madata)***2mean(madata)
plotdata<-data.frame(lambda<-seq(1,20,1))

plotdata$logL<-laply(plotdata$lambda,logL, data=mydata)
head(plotdata)

ggplot(data=plotdata, aes=(x=lambda, y=logL))+
  geom_line()

lambda

my.glm<-glm(Survived ~ Age, data= titanic,family=binomial(link=logit))


# set function
logL <- function(parm,data){
  beta0<-parm[1]
  beta1<- parm[2]
  theta<-beta0+beta1*data$Age
  ps<-1/(1-exp(-theta))
  py<-ps**(data$Survived) * (1-ps)**(1-data$Survived)
  logLsum(log(py), na.rm=TRUE)
  return(logL)
}

# test function
logl(c(-1,0),data=titanic,negll=TRUE)

titanic$Age

mymle<-nlm(logL)


+
  annotate("point", x=mle$estimate, y=-mle$minimum, color="red", size=5)+
  annotate("point", x=mle$ci["LCL"],y=log.like(mle$ci["LCL"],data=my.data), 
           color="blue", size=5, shape=2)+
  annotate("point", x=mle$ci["UCL"],y=log.like(mle$ci["UCL"],data=my.data), 
           color="blue", size=5, shape=2)+
  annotate("point", x=mle2.ci["2.5 %"],y=log.like(mle2.ci["2.5 %"],data=my.data), 
           color="gold", size=5, shape=3)+
  annotate("point", x=mle2.ci["97.5 %"],y=log.like(mle2.ci["97.5 %"],data=my.data), 
           color="gold", size=5, shape=3)+
  annotate("text", x=mle$estimate, y=-mle$minimum - 10,label=paste("MLE:", round(mle$estimate,3)))+
  annotate("text", x=mle$estimate, y=-mle$minimum - 11, label=paste("SE  :", round(mle$se,3)))+
  annotate("text", x=mle$estimate, y=-mle$minimum - 12, 
           label=paste("A CI (nlm):(",paste(round(mle$ci,3),sep=",",collapse=","),")",sep=""))+
  annotate("text", x=mle$estimate, y=-mle$minimum - 13, 
           label=paste("A CI (mle2):(",paste(round(mle2.ci,3),sep=",",collapse=","),")",sep=""))
