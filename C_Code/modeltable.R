dm<-read.csv("mtgreedymean.csv")
dv<-read.csv("mtgreedyvariance.csv")
du<-read.csv("mtucb.csv")
dat<-read.csv("kwgdata.csv")

library(plyr)

dat<-ddply(dat, ~id, summarize, age=agegroup[1])
#parameters
dp<-data.frame(estimate=exp(c(du$par1, du$par2,du$par3)), id=rep(du$id, 3),age=rep(rep(dat$age, each=8),3), param=rep(c("lambda", "beta", "tau"), each=nrow(du)))
dp<-subset(dp, estimate<5)

ddply(ddply(dp, ~param+id, summarize, p=mean(estimate)), ~param, summarize, m=median(p))
ddply(ddply(dp, ~param+age+id, summarize, p=mean(estimate)), ~age+param, summarize, m=median(p))

dp<-data.frame(estimate=c(du$r2, du$X.2), id=rep(du$id, 2),age=rep(rep(dat$age, each=8),2), param=rep(c("r2", "ll"), each=nrow(du)))
ddply(ddply(dp, ~param+id, summarize, p=mean(estimate)), ~param, summarize, m=median(p))
ddply(ddply(dp, ~param+age+id, summarize, p=mean(estimate)), ~age+param, summarize, m=median(p))


dp<-data.frame(estimate=exp(c(dv$par1, dv$par3)), id=rep(dv$id, 2),age=rep(rep(dat$age, each=8),2), param=rep(c("lambda", "tau"), each=nrow(dv)))
dp<-subset(dp, estimate<5)

ddply(ddply(dp, ~param+id, summarize, p=mean(estimate)), ~param, summarize, m=median(p))
ddply(ddply(dp, ~param+age+id, summarize, p=mean(estimate)), ~age+param, summarize, m=median(p))

dp<-data.frame(estimate=c(dv$r2, dv$X.2), id=rep(dv$id, 2),age=rep(rep(dat$age, each=8),2), param=rep(c("r2", "ll"), each=nrow(dv)))
ddply(ddply(dp, ~param+id, summarize, p=mean(estimate)), ~param, summarize, m=median(p))
ddply(ddply(dp, ~param+age+id, summarize, p=mean(estimate)), ~age+param, summarize, m=median(p))


dp<-data.frame(estimate=exp(c(dm$par1, dm$par3)), id=rep(dm$id, 2),age=rep(rep(dat$age, each=8),2), param=rep(c("lambda", "tau"), each=nrow(dm)))
dp<-subset(dp, estimate<5)

ddply(ddply(dp, ~param+id, summarize, p=mean(estimate)), ~param, summarize, m=median(p))
ddply(ddply(dp, ~param+age+id, summarize, p=mean(estimate)), ~age+param, summarize, m=median(p))

dp<-data.frame(estimate=c(dm$r2, dm$X.2), id=rep(dm$id, 2),age=rep(rep(dat$age, each=8),2), param=rep(c("r2", "ll"), each=nrow(dm)))
ddply(ddply(dp, ~param+id, summarize, p=mean(estimate)), ~param, summarize, m=median(p))
ddply(ddply(dp, ~param+age+id, summarize, p=mean(estimate)), ~age+param, summarize, m=median(p))
