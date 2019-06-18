gm<-read.csv("rbfgreedymean.csv")
gv<-read.csv("rbfgreedyvariance.csv")
gu<-read.csv("rbfucb.csv")
mm<-read.csv("mtgreedymean.csv")
mv<-read.csv("mtgreedyvariance.csv")
mu<-read.csv("mtucb.csv")


m<-matrix(0, nrow=160, ncol=6)
r2<-ddply(gm, ~id, summarize, r2=mean(r2))$r2
m[,1]<-(1-r2)*25*log(1/64)
r2<-ddply(gv, ~id, summarize, r2=mean(r2))$r2
m[,2]<-(1-r2)*25*log(1/64)
r2<-ddply(gu, ~id, summarize, r2=mean(r2))$r2
m[,3]<-(1-r2)*25*log(1/64)
r2<-ddply(mm, ~id, summarize, r2=mean(r2))$r2
m[,4]<-(1-r2)*25*log(1/64)
r2<-ddply(mv, ~id, summarize, r2=mean(r2))$r2
m[,5]<-(1-r2)*25*log(1/64)
r2<-ddply(mu, ~id, summarize, r2=mean(r2))$r2
m[,6]<-(1-r2)*25*log(1/64)
m
write.csv(m, "evidence.csv")


dat<-read.csv("kwgdata.csv")
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)
dat<-ddply(dat, ~id, summarise, age=agegroup[1], cond=cond[1])

m1<-m[dat$age=="7-8",]
write.csv(m1, "evidenceyoungkids.csv")

m3<-m[dat$age==">18",]
write.csv(m3, "evidenceadults.csv")
          