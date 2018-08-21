#Simulation of BMT data
#Eric Schulz, March 2018

#housekeeping
rm(list=ls())

#source of modeling code
source("models.R")

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'lsr', 'BayesFactor', 'matrixcalc')
#load them
lapply(packages, require, character.only = TRUE)

#two environments
environments1 <- fromJSON("kernelRough.json")
environments2 <- fromJSON("kernelSmooth.json")

#participants' data
myjson<-fromJSON("kwg.json")

#exctract environments
roughEnvironments <- lapply(environments1, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1'))))
smoothEnvironments <- lapply(environments2, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1' ))))

#loop through the counts
for (k in 1:myjson$count){
  #initialize data frame
  d<-data.frame(x1=numeric(), x2=numeric(), y=numeric())
  for (i in 2:9){
    #enrionment
    env<-myjson$records$data$envOrder[[k]]
    #condition
    condition<-myjson$records$data$condition[k]
    #smooth or rough
    if(condition==0){ee<-data.frame(smoothEnvironments[[env[i]+1]])}
    if(condition==1){ee<-data.frame(roughEnvironments[[env[i]+1]])}
    #scale
    ee$y<-ee$y*myjson$records$data$scale[[k]][i]+5
    #bind
    d<-rbind(d, data.frame(x1=ee$x1, x2=ee$x2, y=ee$y))
  }
  #add round
  d$round<-rep(1:8, each=64)
  #add it
  d$id<-k
  #age
  d$age<-myjson$records$data$age[k]
  #condition
  d$condition<-condition
  #concatenate or create
  if (k==1){dat<-d}
  if (k>1){dat<-rbind(dat, d)}
}

#first 10 was us!
dat<-subset(dat, id>10)
#at least 7
dat<-subset(dat, age>=7)
#new ids
dat$id<-rep(1:160, each=64*8)

#get the RBF results
dbmt<-read.csv("bmt.csv")

dbmt$error<-ave(dbmt$par1, dbmt$id, FUN=function(x){mean(x[exp(x)<5])})
dbmt$beta<-ave(dbmt$par2, dbmt$id, FUN=function(x){mean(x[exp(x)<5])})
dbmt$tau<-ave(dbmt$par3, dbmt$id, FUN=function(x){mean(x[exp(x)<5])})
dbmt$error<-ifelse(is.na(dbmt$error), mean(dbmt$error, na.rm=TRUE), dbmt$error)
dbmt$beta<-ifelse(is.na(dbmt$beta), mean(dbmt$beta, na.rm=TRUE), dbmt$beta)
dbmt$tau<-ifelse(is.na(dbmt$tau), mean(dbmt$tau, na.rm=TRUE), dbmt$tau)

dd<-ddply(dbmt, ~id,summarize, tau=mean(exp(tau)),
          beta=mean(exp(beta)), error=mean(exp(error)))
write.csv(dd, "bmtucbparams.csv")
#lambda
dat$error<-rep(dbmt$error, each=64)
#beta
dat$beta<-rep(dbmt$beta, each=64)
#tau
dat$tau<-rep(dbmt$tau, each=64)

#frame to collect observations
dcollect<-data.frame(id=numeric(), age=numeric(), condition=numeric(), round=numeric(), trial=numeric(), x=numeric(), y=numeric(), z=numeric())
#loop through participants
for (nid in 1:160){
  #loop through rounds
  for (nround in 1:8){
    #get parameters for participant on that round
    dp<-subset(dat, round==nround & id==nid)
    #random initialization as observation t=0
    ind<-sample(1:64,1)
    #X matrix
    X<-as.matrix(dp[ind,1:2])
    #y matrix
    y<-as.matrix(dp[ind,3])
    #X-start, i.e. all possible observations
    Xstar<-as.matrix(dp[,1:2])
    #get lambda
    error<-exp(dp$error[1])
    #get beta
    beta<-exp(dp$beta[1])
    #get tau
    tau<-exp(dp$tau[1])
    #create a parameter vector
    parVec <- c(error) 
    #kernel is RBF
    prevPost <- NULL
    #loop through trials
    for (trial in 1:25){
      #output by GP with particular parameter settings
      #don't forget mean centering and standardization
      out<-bayesianMeanTracker(x=t(X[trial]), y=(y[trial]-25)/50, theta=parVec, prevPost = prevPost)
      prevPost<-out
      #utility vector by UCB
      utilityVec<-ucb(out, beta)
      #avoid overflow
      utilities <- utilityVec - max(utilityVec)
      #softmaximization
      p <- exp(utilities/tau)
      #probabilities
      p <- p/colSums(p)
      #numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #index is sampled proprotionally to softmaxed utitily vector
      ind<-sample(1:64, 1, prob=p)
      #bind X-observations
      X<-rbind(X, as.matrix(dp[ind,1:2]))
      #bind y-observations
      y<-rbind(y, as.matrix(dp[ind,3]))
    }
    #dummy data frame
    dummy<-data.frame(id=rep(nid, 26), age=rep(dp$age[1], 26), condition=rep(dp$condition[1], 26),
                      round=rep(nround, 26), trial=0:25, x=as.numeric(X[,1]), y=as.numeric(X[,2]),
                      z=as.numeric(y))
    #bind them
    dcollect<-rbind(dcollect, dummy)
  }
}
#save csv of learning curves
write.csv(dcollect, "bmtrecoverydata.csv")

dat<-dcollect
#age groups are 7-8, 9-11, and adults
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)

#conditions are smooth and rough
dat$Condition<-ifelse(dat$cond==1, "Rough", "Smooth")
se<-function(x){sd(x)/sqrt(length(x))}

#summarize by agegroup, trial, and condition
dd<-ddply(dat, ~agegroup+trial+Condition, summarize, mu=mean(z), se=se(z))
#rough and smooth
dd$Condition<-factor(dd$Condition, levels = c("Smooth", "Rough"))
#call it Age again
dd$Age<-dd$agegroup
#factors...
dd$Age<-factor(dd$agegroup, levels=c("7-8", "9-11", ">18"))
pd <- position_dodge(.1)
#palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
fontsize<-12
##Learning over trials by age and condition
p3<-ggplot(dd, aes(x=trial, y=mu, group=interaction(Condition, Age), col=Age, linetype=Condition)) +
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=.8, size = .5, position=pd, linetype=1) +
  #line
  geom_line(size=0.8, position=pd)+
  #labs
  ylab("Reward")+xlab("Trial")+
  #scales
  scale_y_continuous(breaks=c(25,30,35,40,45))+
  scale_x_continuous(breaks=seq(0,26,5))+
  #theme
  theme_minimal()+
  #color scheme
  scale_color_manual(values = cbPalette[c(7,6,1)])+
  #theme
  theme(text = element_text(size=fontsize,  family="sans")) +
  #titlte
  ggtitle("C: Learning curves")+
  #theme
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))
p3

dopt<-expand.grid(x=0:7, y=0:7)
dat$chosen<-0
for (i in 1:nrow(dat)){
  dat$chosen[i]<-which(dopt$x==dat$x[i] & dopt$y == dat$y[i])
}
head(dat, 200)
dfinal<-data.frame(id=dat$id, round=dat$round, trial=dat$trial, 
                   x=dat$x, y=dat$y, z=dat$z, chosen=dat$chosen)
write.csv(dfinal, "recoverbmt.csv")
