#Model comparison
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

cohensd.ci <- function(d, n1, n2, ci = 0.95) {
  t <- d * sqrt((n1 * n2)/(n1 + n2))
  capture.output(
    fit <- compute.es::tes(t = t, n.1 = n1, n.2 = n2, level = 100 * ci),
    file = "NUL"
  )
  c(lower.ci = fit$l.d, upper.ci = fit$u.d)
}
#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'BayesFactor', 'lsr')
lapply(packages, require, character.only = TRUE)

#read in data
myjson<-fromJSON("kwg.json")

#RBF-results
drbf<-read.csv("rbf.csv")
#meant tracker results
dbmt<-read.csv("bmt.csv")

#get pseudo-r2
drbf$r2<-(1-drbf$X.2/(-25*log(1/64)))
dbmt$r2<-(1-dbmt$X.2/(-25*log(1/64)))

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

#mark model names
drbf$model<-"RBF"
dbmt$model<-"BMT"

#combine to new data frame
d<-data.frame(r2=c(drbf$r2, dbmt$r2), model=c(drbf$model, dbmt$model), id=c(drbf$id, dbmt$id))

#initialize
d$condition<-0
d$age<-0

#loop through d to match age and condition
for (i in 1:nrow(d)){
  d$condition[i]<-dat$cond[d$id[i]==dat$id]
  d$age[i]<-dat$age[d$id[i]==dat$id]
}

#create a data frame for parameters of GP-UCB
dp<-data.frame(estimate=exp(c(drbf$par1, drbf$par2,drbf$par3)), 
               id=rep(drbf$id, 3),
               param=rep(c("lambda", "beta", "tau"), each=nrow(drbf)))

#initialize condition and age
dp$condition<-0
dp$age<-0

#get condition and age
for (i in 1:nrow(dp)){
  dp$condition[i]<-dat$cond[dp$id[i]==dat$id]
  dp$age[i]<-dat$age[dp$id[i]==dat$id]
}

#get model performance per id while keeping age and condition in the frame
dm<-ddply(d, ~model+id+age+condition, summarize, mu=mean(r2))

#test the difference overall
t.test(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu, var.equal = TRUE)
cohensD(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)
ttestBF(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)

#test the difference for adults
dm<-ddply(subset(d,age==">18"),  ~model+id+age+condition, summarize, mu=mean(r2))
t.test(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu, var.equal = TRUE)
cohensD(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)
ttestBF(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)

#test the difference for older children
dm<-ddply(subset(d,age=="9-11"),  ~model+id+age+condition, summarize, mu=mean(r2))
t.test(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu, var.equal = TRUE)
cohensD(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)
ttestBF(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)

#test the difference for older children
dm<-ddply(subset(d,age=="7-8"),  ~model+id+age+condition, summarize, mu=mean(r2))
t.test(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu, var.equal = TRUE)
cohensD(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)
ttestBF(subset(dm, model=="RBF")$mu-subset(dm, model=="BMT")$mu)

#get only RBF for the different age groups
dm<-ddply(subset(d,model=="RBF"), ~id+age+condition, summarize, mu=mean(r2))

#difference of GP-UCB between adults and older children
t.test(subset(dm, age==">18")$mu, subset(dm, age=="9-11")$mu, var.equal = TRUE)
cohensD(subset(dm, age==">18")$mu, subset(dm, age=="9-11")$mu)
ttestBF(subset(dm, age==">18")$mu, subset(dm, age=="9-11")$mu)

#difference of GP-UCB between younger and older children
t.test(subset(dm, age=="7-8")$mu, subset(dm, age=="9-11")$mu, var.equal = TRUE)
cohensD(subset(dm, age=="7-8")$mu, subset(dm, age=="9-11")$mu)
ttestBF(subset(dm, age=="7-8")$mu, subset(dm, age=="9-11")$mu)

#############################################################################################
#PARAMETER COMPARISON
#############################################################################################

#Source Mann-Whitney-U-BF
source("mannwhitbf.R")

kendallci<-function(x,y){
  tau<-cor(x,y, method="kendall")
  d<-rep(0, 10000)
  for (i in 1:10000){
    ind<-sample(1:length(x), replace=TRUE)
    d[i]<-tau-cor(x[ind],y[ind], method="kendall")
  }
  d<-quantile(d, probs = c(0.025, 0.975))
  return(tau+d)
}

#LAMBDA:
#get the estimates
dlambda<-subset(dp, param=="lambda")
#mean per id as well as age
dage<-ddply(dlambda, ~id+age,summarize, mu=mean(estimate))
#Mann-Whitney-U of adults vs. older children
wilcox.test(subset(dage, age==">18")$mu, subset(dage, age=="9-11")$mu)
#get a subset
dd<-subset(dage, age %in% c(">18", "9-11"))
#rank correlation as effect size
cor(ifelse(dd$age==">18",1,0), dd$mu, method="kendall")
kendallci(ifelse(dd$age==">18",1,0), dd$mu)

#Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age==">18")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)

#Mann-Whitney-U of younger vs. older children
wilcox.test(subset(dage, age=="9-11")$mu, subset(dage, age=="7-8")$mu)
#subset of data frame only with kids
dd<-subset(dage, age %in% c("7-8", "9-11"))
#rank correlation of effect size
cor(ifelse(dd$age=="9-11",1,0), dd$mu, method="kendall")
kendallci(ifelse(dd$age=="9-11",1,0), dd$mu)

#Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age=="7-8")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)


#BETA:
#get the estimate
dbeta<-subset(dp, param=="beta")
#mean per id and age
dage<-ddply(dbeta, ~id+age,summarize, mu=mean(estimate))
#Mann-Whitney-U test for adults vs. old kids
wilcox.test(subset(dage, age==">18")$mu, subset(dage, age=="9-11")$mu)
#subset of data frame
dd<-subset(dage, age %in% c(">18", "9-11"))
#rank correlation as effect size
cor(ifelse(dd$age==">18",1,0), dd$mu, method="kendall")
kendallci(ifelse(dd$age==">18",1,0), dd$mu)

#get Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age==">18")$mu, subset(dd, age=="9-11")$mu)
hist(outsim$deltaSamples)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
#problem is that the effect is so big, it can't estimate the density at x=0
#we therefore use the even weaeker x=0.5 to approximate
#it's already much more than 100 for this already, so doesn't matter too much
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(.05)
print(BF)

#Mann-Whitney-U 
wilcox.test(subset(dage, age=="9-11")$mu, subset(dage, age=="7-8")$mu)
#subset of children
dd<-subset(dage, age %in% c("7-8", "9-11"))
#rank correlation for effect size
cor(ifelse(dd$age=="9-11",1,0), dd$mu, method="kendall")
kendallci(ifelse(dd$age=="9-11",1,0), dd$mu)

#Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age=="7-8")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)


#tau-parameter
dtau<-subset(dp, param=="tau")
#get per age
dage<-ddply(dtau, ~id+age,summarize, mu=mean(estimate))
#do ranks
#ANOVA is impossible so, we report the biggest BF, which was for 9-11 vs adults
dd<-subset(dage, age %in% c(">18", "9-11"))
cor(ifelse(dd$age==">18",1,0), dd$mu, method="kendall")
kendallci(ifelse(dd$age==">18",1,0), dd$mu)

outsim<-rankSumGibbsSampler(subset(dage, age=="9-11")$mu, subset(dage, age==">18")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)
