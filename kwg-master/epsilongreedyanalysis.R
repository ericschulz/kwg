#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'gridExtra', 'ggjoy', "tikzDevice")
lapply(packages, require, character.only = TRUE)

d<-read.csv("rbfucbepsilon.csv")

#read in data
myjson<-fromJSON("kwg.json")

#initialize empty data frame
dat<-data.frame(id=numeric(), cond=numeric(), age=numeric(), x=numeric(), y=numeric(), 
                z=numeric(), trial=numeric(), round=numeric())

#loop through json
for (i in 1:myjson$count){
  #x-y-z
  x<-as.vector(t(myjson$records$data$searchHistory$xcollect[[i]][2:9,]))
  y<-as.vector(t(myjson$records$data$searchHistory$ycollect[[i]][2:9,]))
  z<-as.vector(t(myjson$records$data$searchHistory$zcollect[[i]][2:9,]))
  #condition
  cond<-rep(myjson$records$data$condition[i], length(x))
  #age
  age<-rep(myjson$records$data$age[i], length(x))
  #trial number
  trial<-rep(0:25, 8)
  #round number
  round<-rep(1:8, each=26)
  #id
  id<-rep(i, length(x))
  #dummy frame
  dummy<-data.frame(id=id, cond=cond, age=age, x=x, y=y, z=z, trial=trial, round=round)
  #bind them
  dat<-rbind(dat, dummy)
}

#first 10 were us!
dat<-subset(dat, id>10)
#remove younger than 7
dat<-subset(dat, age>=7)

#age group
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)

#condition
dat$cond<-ifelse(dat$cond==1, "Rough", "Smooth")

#get everyones agegroup and condition
dat<-ddply(dat, ~id, summarize, age=agegroup[1], cond=cond[1])
#new ids so they match the model comparison results
dat$id<-1:160

dd<-ddply(d,~id,summarize, tau=median(exp(par3)), beta=median(exp(par2)))
dd$beta
for (i in 1:nrow(dd)){
  Tau<-c(rep(dd$tau[i], 63),exp(5))
  Tau<-Tau/sum(Tau)
  dd$tau[i]<-Tau[1]/63
}

dd$age<-dat$age
tapply(dd$tau,dd$age, mean )

dd$adult<-ifelse(dd$age==">18", 1, 0)
dd$taurank<-rank(dd$tau)
dd$betarank<-rank(dd$beta)
m<-glm(adult~betarank+taurank, data=dd, family="binomial")
summary(m)
cor(dd$adult, dd$betarank)
cor(dd$adult, dd$taurank)

d$r2<-1-d$X.2/(-25*log(1/64))
du<-read.csv("rbfucb.csv")

ds<-ddply(du, ~id, summarize, r=mean(r2))
dg<-ddply(d, ~id, summarize, r=mean(r2))
t.test(ds$r-dg$r)
library(BayesFactor)
ttestBF(ds$r-dg$r)
library(lsr)
cohensD(ds$r-dg$r)
mean(dg$r)

source("mannwhitbf.R")
wilcox.test(subset(dd, age==">18")$beta, subset(dd, age=="9-11")$beta)
da<-subset(dd, age %in% c(">18", "9-11"))
#rank correlation as effect size
cor(ifelse(da$age==">18",1,0), da$beta, method="kendall")
outsim<-rankSumGibbsSampler(subset(da, age==">18")$beta, subset(da, age=="9-11")$beta)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(.3)
print(BF)

wilcox.test(subset(dd, age=="7-8")$tau, subset(dd, age=="9-11")$tau)
da<-subset(dd, age %in% c("7-8", "9-11"))
#rank correlation as effect size
cor(ifelse(da$age=="7-8",1,0), da$tau, method="kendall")
outsim<-rankSumGibbsSampler(subset(da, age=="7-8")$tau, subset(da, age=="9-11")$tau)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(.0)
print(BF)
