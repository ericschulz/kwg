setwd("/home/hanshalbe/kwg")
du<-read.csv("rbfucb.csv")

d<-read.csv("kwgdata.csv")

d<-ddply(d, ~id, summarize, age=age[1])
du$lambda<-exp(du$par1)
du$beta<-exp(du$par2)
du$tau<-exp(du$par3)
mymean<-function(x){mean(x[x<=5])}

dd<-ddply(du, ~id,summarize, lambda=mymean(lambda), beta=mymean(beta), tau=mymean(tau))
dd$age<-d$age
dd<-subset(dd, age>=12)

library(BayesFactor)
cor.test(dd$lambda, dd$age)
correlationBF(dd$lambda, dd$age)

cor.test(dd$beta, dd$age)
correlationBF(dd$beta, dd$age)

cor.test(dd$tau, dd$age)
correlationBF(dd$tau, dd$age)
