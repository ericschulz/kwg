#Judgement analysis
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'lsr', 'BayesFactor')
lapply(packages, require, character.only = TRUE)
library(effsize)

#rough and smooth environments
environments1 <- fromJSON("kernelRough.json")
environments2 <- fromJSON("kernelSmooth.json")

#split up json for rough and smooth enviornments
roughEnvironments <- lapply(environments1, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1'))))
smoothEnvironments <- lapply(environments2, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1' ))))


#data
myjson<-fromJSON("kwg.json")

#initialize empty frame
dat<-data.frame(id=numeric(), age=numeric(), x=numeric(), y=numeric(), est=numeric(), 
                cer=numeric(), cond=numeric(), truth=numeric(), chosen=numeric())

#loop through json
for (i in 1:myjson$count){
  #x-y-locations
  x<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$x)
  y<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$y)
  #estimates
  est<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$givenValue)
  #uncertainties
  cert<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$howSecure)
  #environments
  env<-myjson$records$data$envOrder[[i]][10]
  #condition
  condition<-myjson$records$data$condition[i]
  #get smooth
  if(condition==0){ee<-data.frame(smoothEnvironments[[env+1]])}
  if(condition==1){ee<-data.frame(roughEnvironments[[env+1]])}
  #initialize truth
  truth<-rep(0, length(x))
  #loop over truth
  for (k in seq_along(truth)){
    #get truth
    truth[k]<-ee$y[x[k]==ee$x1 & y[k]==ee$x2]
  }
  #truth rescaled
  truth<-truth*myjson$records$data$scale[[i]][10]+5
  #initialize chosen
  chosen<-rep(0, 5)
  #which one did they choose
  ch<-myjson$records$data$bonusLevel$finalChosenCell[i,]
  #mark chosen one by a 1
  chosen[x==ch$x & y ==ch$y]<-1
  #dummy frame
  dummy<-data.frame(id=rep(i, 5), age=rep(myjson$records$data$age[i], 5), x=x, y=y,
                    est=est, cer=cert, cond=rep(condition, 5), truth=truth, chosen=chosen)
  #bind data frames together
  dat<-rbind(dat, dummy)
}

#first 10 was us!
dat<-subset(dat, id>10)
#age has to be at least 7
dat<-subset(dat, age>=7)

#get absolute error
dat$error<-abs(dat$est-dat$truth)

#create age group
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)

#Condition
dat$cond<-ifelse(dat$cond==1, "Rough", "Smooth")

#Error by condition
de<-ddply(dat, ~id+cond, summarize, mu=mean(error))
#Rough vs. smooth
t.test(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu, var.equal = TRUE)
cohensD(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu)
ttestBF(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu)
cohen.d(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu)
#Error by age
dage<-ddply(dat, ~id+agegroup,summarize, mu=mean(error))

#adults vs. old children
t.test(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu, var.equal = TRUE)
cohensD(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu)
ttestBF(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu)
cohen.d(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu)

#old children vs. young children
t.test(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu, var.equal = TRUE)
cohensD(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu)
ttestBF(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu)
cohen.d(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu)


#Certainty judgements
de<-ddply(dat, ~id+cond, summarize, mu=mean(cer))
#Smooth vs. Rough
t.test(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu, var.equal = TRUE)
cohensD(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu)
ttestBF(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu)
cohen.d(subset(de, cond=="Rough")$mu, subset(de, cond=="Smooth")$mu)


#certainty by age group
dage<-ddply(dat, ~id+agegroup,summarize, mu=mean(cer))
#make age a factor
dage$agegroup<-as.factor(dage$agegroup)
#ANOVA Bayes Factor
anovaBF(mu~agegroup, data=dage)

#Spearman correlation between error and certainty
d<-ddply(dat, ~id+agegroup, summarize, co=cor(cer, error, method="spearman"))
d$co<-ifelse(is.na(d$co),0,d$co)
t.test(d$co)
ttestBF(d$co)

#difference of correlation by age group
d$agegroup<-factor(d$agegroup)
anovaBF(co~agegroup, data=d)

#standardized characeristics of chosen options
ds<-ddply(dat, ~id+agegroup, summarize, cer=cer[chosen==1]/sum(cer), mu=est[chosen==1]/sum(est))
ds$agegroup<-factor(ds$agegroup)

#difference in predicted value
anovaBF(mu~agegroup, data=ds)

#difference in uncertainty
ds$cer<-ifelse(is.na(ds$ce), 0.2, ds$ce)

#overall
anovaBF(cer~agegroup, data=ds)

#between the two children groups
t.test(subset(ds, agegroup=="9-11")$cer, subset(ds, agegroup=="7-8")$cer, var.equal = TRUE)
cohensD(subset(ds, agegroup=="9-11")$cer, subset(ds, agegroup=="7-8")$cer)
ttestBF(subset(ds, agegroup=="9-11")$cer, subset(ds, agegroup=="7-8")$cer)
cohen.d(subset(ds, agegroup=="9-11")$cer, subset(ds, agegroup=="7-8")$cer)

#between the adults and young kids
t.test(subset(ds, agegroup==">18")$cer, subset(ds, agegroup=="7-8")$cer, var.equal = TRUE)
cohensD(subset(ds, agegroup==">18")$cer, subset(ds, agegroup=="7-8")$cer)
ttestBF(subset(ds, agegroup==">18")$cer, subset(ds, agegroup=="7-8")$cer)
cohen.d(subset(ds, agegroup==">18")$cer, subset(ds, agegroup=="7-8")$cer)

