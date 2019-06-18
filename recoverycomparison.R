
library(plyr)
library(BayesFactor)
library(lsr)
library(effsize)
#Read recovery data files
mm<-read.csv("bmtrecoverbmt.csv")
mg<-read.csv("bmtrecovergp.csv")
gm<-read.csv("gprecoverbmt.csv")
gg<-read.csv("gprecovergp.csv")

#
#mean(mm$r2)
#mean(gm$r2)
#mean(gg$r2)
#mean(mg$r2)

mm$id<-mg$id<-gm$id<-gg$id<-rep(1:160, each=8)

#sum negative log likelihood over 8 crossvalidation slices 
mm<-ddply(mm,  ~id,summarize, nll=sum(X.2))
gm<-ddply(gm,  ~id,summarize, nll=sum(X.2))
mg<-ddply(mg,  ~id,summarize, nll=sum(X.2))
gg<-ddply(gg,  ~id,summarize, nll=sum(X.2))

#compute R2
mm$r2<-(1-mm$nll/(-8*25*log(1/64))) #random chance is 1/64 for each trial, for 25 trials, in 8 rounds
mg$r2<-(1-mg$nll/(-8*25*log(1/64)))
gm$r2<-(1-gm$nll/(-8*25*log(1/64)))
gg$r2<-(1-gg$nll/(-8*25*log(1/64)))

#BMT generated data
mean(gm$r2) 
mean(mm$r2)
t.test(mm$r2-gm$r2)
cohensD(mm$r2-gm$r2)
ttestBF(mm$r2-gm$r2)
sum(mm$r2-gm$r2>=0) #how many simualted participants are best described by BMT vs. GP
cohen.d(mm$r2, gm$r2, paired=TRUE)

#GP generated data
t.test(gg$r2-mg$r2)
cohensD(gg$r2-mg$r2)
ttestBF(mm$r2-gm$r2)
cohen.d(gg$r2, mg$r2, paired=TRUE)
sum(gg$r2-mg$r2>=0)
