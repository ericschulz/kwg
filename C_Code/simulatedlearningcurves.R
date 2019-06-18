#Simulation of learning curves
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
drbf<-read.csv("rbf.csv")

#lambda
dat$lambda<-rep(drbf$par1, each=64)
#beta
dat$beta<-rep(drbf$par2, each=64)
#tau
dat$tau<-rep(drbf$par3, each=64)

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
    lambda<-exp(dp$lambda[1])
    #get beta
    beta<-exp(dp$beta[1])
    #get tau
    tau<-exp(dp$tau[1])
    #create a parameter vector
    parVec <- c(lambda, lambda, 1, .0001) 
    #kernel is RBF
    k<-rbf
    #loop through trials
    for (trial in 1:25){
      #output by GP with particular parameter settings
      #don't forget mean centering and standardization
      out<-gpr(X.test=Xstar, theta=parVec, X=X, Y=(y-25)/50, k=k)
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
write.csv(dcollect, "learningcurves.csv")