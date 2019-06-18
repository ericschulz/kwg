#Simulation of learning curves
#Eric Schulz, March 2018

#housekeeping
rm(list=ls())

#source of modeling code
source("kwg/models.R")

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'lsr', 'BayesFactor', 'matrixcalc')
#load them
lapply(packages, require, character.only = TRUE)

#two environments
environments1 <- fromJSON("kernelRough.json")
environments2 <- fromJSON("kernelSmooth.json")


#exctract environments
roughEnvironments <- lapply(environments1, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1'))))
smoothEnvironments <- lapply(environments2, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1' ))))


rough<-as.data.frame(roughEnvironments[[1]])
smooth<-as.data.frame(smoothEnvironments[[1]])

for (i in 2:40){
  rough<-rbind(rough,as.data.frame(roughEnvironments[[i]]))
  smooth<-rbind(smooth,as.data.frame(smoothEnvironments[[i]]))
}
rough$en<-smooth$en<-rep(1:40, each=64)

rough$y<-rough$y*35+5
smooth$y<-smooth$y*35+5

dparams<-expand.grid(lambda=c(0.0001, seq(0.05,2,0.05)), beta=c(0.0001, seq(0.05,2,0.05)), tau=0.01)
dparams$smooth<-dparams$rough<-0
Xstar<-cbind(smooth$x1[1:64], smooth$x2[1:64])
k<-rbf
for (i in 1:nrow(dparams)){
  lambda<-dparams$lambda[i]
  beta<-dparams$beta[i]
  tau<-dparams$tau[i]
  parVec <- c(lambda, lambda, 1, .0001) 

  
  musmooth<-murough<-numeric()
  for (round in 1:100){
    
    enselect<-sample(1:40, 1)
    environ<-subset(smooth, en==enselect)
    ind<-sample(1:64,1)
    #X matrix
    X<-cbind(environ$x1[ind], environ$x2[ind])
    #y matrix
    y<-as.matrix(environ$y[ind])
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
      X<-rbind(X, cbind(environ$x1[ind], environ$x2[ind]))
      #bind y-observations
      y<-rbind(y, as.matrix(environ$y[ind]))
    }
    musmooth<-c(musmooth, mean(y))
    
    
    environ<-subset(rough, en==enselect)
    ind<-sample(1:64,1)
    #X matrix
    X<-cbind(environ$x1[ind], environ$x2[ind])
    #y matrix
    y<-as.matrix(environ$y[ind])
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
      X<-rbind(X, cbind(environ$x1[ind], environ$x2[ind]))
      #bind y-observations
      y<-rbind(y, as.matrix(environ$y[ind]))
    }
    murough<-c(murough, mean(y))
  }
  dparams$smooth[i]<-mean(musmooth)
  dparams$rough[i]<-mean(murough)
  print(i)
}

write.csv(dparams, "optimalparameters2.csv")

head(dparams)
library(ggplot2)
library(viridis)
dparams<-read.csv("optimalparameters2.csv")

dd<-data.frame(beta=rep(dparams$beta, 2),
               lambda=rep(dparams$lambda, 2),
               mu=c(dparams$rough,dparams$smooth),
               cond=rep(c("Rough", "Smooth"), each=nrow(dparams)))
               
dd1<-subset(dd, cond=="Rough")
dd1<-subset(dd1, mu>=(max(dd1$mu)-0.4))


dd2<-subset(dd, cond=="Smooth")
dd2<-subset(dd2, mu>=(max(dd2$mu)-0.36))
median(dd1$lambda)
range(dd1$lambda)
median(dd2$lambda)
range(dd2$lambda)
nrow(dd1)
nrow(dd2)
median(dd1$beta)
range(dd1$beta)
median(dd2$beta)
range(dd2$beta)


dp<-read.csv("kwg/gpucbparams.csv")
da<-ddply(read.csv("kwg/kwgdata.csv"), ~id, summarize, age=agegroup[1], cond=cond[1])
dp$age<-dp$cond<-0
for (i in 1:nrow(dp)){
  dp$cond[i]<-paste(da$cond[dp$id[i]==da$id])
  dp$age[i]<-paste(da$age[dp$id[i]==da$id])
} 

mymean<-function(x) {mean(x[x<=5])}

dp<-ddply(dp, ~cond+age, summarize, lambda=mymean(lambda), beta=mymean(beta))
dp$mu<-mean(dd$mu)
dp$Age<-factor(dp$age, levels=c("7-8", "9-11", ">18"))


cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



p1 <- ggplot(dd, aes(y=beta, x=lambda, fill=mu)) +
  scale_fill_viridis(name="Mean\nReward")+
  geom_tile(color = "black") +
  geom_point(data=dp,aes(x=lambda,y=beta, col=Age, shape=Age) , fill="black", size=7) +
  scale_y_continuous( expand = c(0, 0), breaks=seq(0.2,2,.2))+
  scale_x_continuous( expand = c(0, 0), breaks=seq(0.2,2,.2))+
  scale_color_manual(values= c(cbPalette[c(7)],"skyblue", "grey40"))+
  scale_shape_manual(values=c(15,19,17))+
  xlab(expression(lambda))+ylab(expression(beta))+
  ggtitle("Simulated performance")+
  theme_minimal()+
  facet_wrap(~cond)+
  theme(text = element_text(size=20,  family="sans"))
p1
pdf("simperf.pdf", width=12, height=6)
p1
dev.off()

getwd()
dparams<-subset(dparams, beta>0.2)
dparams[which.max(dparams$smooth),]