library(plyr)

d<-read.csv("swappedparams.csv")
d$lambda<-exp(d$par1)
d$beta<-exp(d$par2)
d$tau<-exp(d$par3)

dat<-read.csv("kwgdata.csv")
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)
dat<-ddply(dat, ~id, summarise, age=agegroup[1], cond=cond[1])
dat$id<-1:160


#####################################
#TESTS
#####################################

#intitialize condition
d$condition<-0
#initialize age
d$age<-0
#add age and condition
for (i in 1:nrow(d)){
  d$condition[i]<-dat$cond[d$id[i]==dat$id]
  d$age[i]<-dat$age[d$id[i]==dat$id]
}

source("mannwhitbf.R")

de<-d
#LAMBDA:
#get the estimates
#mean per id as well as age
dage<-ddply(de, ~id+age,summarize, mu=mean(lambda))
#Mann-Whitney-U of adults vs. older children
wilcox.test(subset(dage, age==">18")$mu, subset(dage, age=="9-11")$mu)
#get a subset
dd<-subset(dage, age %in% c(">18", "9-11"))
#rank correlation as effect size
cor(ifelse(dd$age==">18",1,0), dd$mu, method="kendall")
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
#Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age=="7-8")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)


#BETA:
#mean per id and age
dage<-ddply(de, ~id+age,summarize, mu=mean(beta))
#Mann-Whitney-U test for adults vs. old kids
wilcox.test(subset(dage, age=="7-8")$mu, subset(dage, age=="9-11")$mu)
#subset of data frame
dd<-subset(dage, age %in% c("7-8", "9-11"))
#rank correlation as effect size
cor(ifelse(dd$age=="7-8",1,0), dd$mu, method="kendall")
#get Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age=="7-8")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)

#Mann-Whitney-U 
wilcox.test(subset(dage, age=="9-11")$mu, subset(dage, age=="7-8")$mu)
#subset of children
dd<-subset(dage, age %in% c("7-8", "9-11"))
#rank correlation for effect size
cor(ifelse(dd$age=="9-11",1,0), dd$mu, method="kendall")
#Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age=="7-8")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)


#tau-parameter
#get per age
dage<-ddply(de, ~id+age,summarize, mu=mean(tau))
#do ranks
wilcox.test(subset(dage, age=="9-11")$mu, subset(dage, age=="7-8")$mu)
#subset of children
dd<-subset(dage, age %in% c("9-11", "7-8"))
#rank correlation for effect size
cor(ifelse(dd$age=="9-11",1,0), dd$mu, method="kendall")


#ANOVA is impossible so, we report the biggest BF, which was for 9-11 vs adults
outsim<-rankSumGibbsSampler(subset(dd, age=="7-8")$mu, subset(dd, age=="9-11")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)
