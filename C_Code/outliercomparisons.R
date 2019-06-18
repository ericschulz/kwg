library(plyr)
library(BayesFactor)
library(lsr)
library(effsize)
du<-read.csv("rbfucb.csv")
dat<-read.csv("kwgdata.csv")

lambda<-exp(du$par1)
beta<-exp(du$par2)
tau<-exp(du$par3)
age<-ddply(dat, ~id, summarize, age=agegroup[1])$age
dall<-data.frame(id=rep(1:160, each=8), age=rep(age, each=8), lambda=lambda, beta=beta, tau=tau)
dall

plot(dall$beta)
outbeta <- boxplot.stats(dall$beta)$out
outtau <- boxplot.stats(dall$tau)$out
outlambda <- boxplot.stats(dall$lambda)$out
length(outlambda)/nrow(dall)
length(outbeta)/nrow(dall)
length(outtau)/nrow(dall)

dall1<-subset(dall, ! beta %in% outbeta)
dall1<-subset(dall, ! tau %in% outtau)

dall1<-subset(dall, lambda < 5)
d<-ddply(dall1, ~age+id, summarize, m=mean(lambda))
t.test(subset(d, age=="9-11")$m, subset(d, age==">18")$m, var.equal = TRUE)
cohensD(subset(d, age=="9-11")$m, subset(d, age==">18")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age==">18")$m) 
cohen.d(subset(d, age=="9-11")$m, subset(d, age==">18")$m) 

t.test(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m, var.equal = TRUE)
cohensD(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)        
cohen.d(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)  

dall1<-subset(dall, beta < 5)
d<-ddply(dall1, ~age+id, summarize, m=mean(beta))
t.test(subset(d, age=="9-11")$m, subset(d, age==">18")$m, var.equal = TRUE)
cohensD(subset(d, age=="9-11")$m, subset(d, age==">18")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age==">18")$m)        
cohen.d(subset(d, age=="9-11")$m, subset(d, age==">18")$m)

t.test(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m, var.equal = TRUE)
cohensD(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)    
cohen.d(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)    


dall1<-subset(dall, tau < 5)
d<-ddply(dall1, ~age+id, summarize, m=mean(tau))
t.test(subset(d, age=="9-11")$m, subset(d, age==">18")$m, var.equal = TRUE)
cohensD(subset(d, age=="9-11")$m, subset(d, age==">18")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age==">18")$m)        
cohen.d(subset(d, age=="9-11")$m, subset(d, age==">18")$m)  

t.test(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m, var.equal = TRUE)
cohensD(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)    
cohen.d(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m) 


dall1<-subset(dall, ! lambda %in% outlambda)
d<-ddply(dall1, ~age+id, summarize, m=mean(lambda))
t.test(subset(d, age=="9-11")$m, subset(d, age==">18")$m, var.equal = TRUE)
cohensD(subset(d, age=="9-11")$m, subset(d, age==">18")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age==">18")$m)        
cohen.d(subset(d, age=="9-11")$m, subset(d, age==">18")$m)


t.test(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m, var.equal = TRUE)
cohensD(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)        
cohen.d(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)


dall1<-subset(dall, ! beta %in% outbeta)
d<-ddply(dall1, ~age+id, summarize, m=mean(beta))
t.test(subset(d, age=="9-11")$m, subset(d, age==">18")$m, var.equal = TRUE)
cohensD(subset(d, age=="9-11")$m, subset(d, age==">18")$m)
cohen.d(subset(d, age=="9-11")$m, subset(d, age==">18")$m)        


t.test(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m, var.equal = TRUE)
cohensD(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)   
cohen.d(subset(d, age=="9-11")$m, subset(d, age=="7-8")$m)         


dall1<-subset(dall, ! beta %in% outtau)
d<-ddply(dall1, ~age+id, summarize, m=mean(tau))
t.test(subset(d, age=="9-11")$m, subset(d, age==">18")$m, var.equal = TRUE)
cohensD(subset(d, age=="9-11")$m, subset(d, age==">18")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age==">18")$m)        
cohen.d(subset(d, age=="9-11")$m, subset(d, age==">18")$m)         


t.test(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m, var.equal = TRUE)
cohensD(subset(d, age=="7-8")$m, subset(d, age=="9-11")$m)
ttestBF(subset(d, age=="9-11")$m, subset(d, age=="9-11")$m)   
cohen.d(subset(d, age=="9-11")$m, subset(d, age=="9-11")$m)         
