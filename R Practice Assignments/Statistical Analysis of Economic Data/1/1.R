rm(list=ls())
setwd("C:\\Users\\Kun\\Desktop\\333\\1")
csdata<-read.csv("testscores_california_1999.csv")

head(csdata)
tail(csdata)
summary(csdata)


summary(csdata$math_sc)
sd(csdata$math_sc)

n<-length(csdata$math_sc)
x_bar<-mean(csdata$math_sc)
s<-sd(csdata$math_sc)
a<-0.01
q<-qt(1-a/2,(n-1)) 
result1<-c(x_bar-q*s/sqrt(n), x_bar+q*s/sqrt(n))
result1

n2<-length(csdata$avginc)
x2_bar<-mean(csdata$avginc)
s2<-sd(csdata$avginc)
a2<-0.41
q2<-qt(1-a2/2,(n2-1)) 
result2<-c(x2_bar-q2*s2/sqrt(n2), x2_bar+q2*s2/sqrt(n2))
result2


model.fit=lm(math_scr~str,data=csdata)
summary(model.fit)
confint(model.fit,'str',level=0.95)

model.fit2=lm(math_scr~str+comp_stu+el_pct+avginc,data=csdata)
summary(model.fit2)

