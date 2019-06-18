#Behavioral Tests
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'jsonlite', 'lsr', 'BayesFactor')
lapply(packages, require, character.only = TRUE)

#read in json
myjson<-fromJSON("kwg.json")

dat<-data.frame(id=numeric(), cond=numeric(), age=numeric(),x=numeric(), y=numeric(), 
                z=numeric(), time=numeric(), trial=numeric(), round=numeric())

#loop through json to create data frame
for (i in 1:myjson$count){
  #x-y-z
  x<-as.vector(t(myjson$records$data$searchHistory$xcollect[[i]][2:9,]))
  y<-as.vector(t(myjson$records$data$searchHistory$ycollect[[i]][2:9,]))
  z<-as.vector(t(myjson$records$data$searchHistory$zcollect[[i]][2:9,]))
  #time
  time<-as.vector(t(myjson$records$data$searchHistory$tscollect[[i]][2:9,]))
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
  #dummy data frame
  dummy<-data.frame(id=id, cond=cond, age=age, x=x, y=y, z=z, time=time, trial=trial, round=round)
  #bind them
  dat<-rbind(dat, dummy)
}

#first 10 was us
dat<-subset(dat, id>10)
#only if older than 7
dat<-subset(dat, age>=7)

#create age group
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)

#condition
dat$cond<-ifelse(dat$cond==1, "Rough", "Smooth")

#test smooth vs. rough
dcond<-ddply(dat, ~id+cond,summarize, mu=mean(z))
t.test(subset(dcond, cond=="Rough")$mu, subset(dcond, cond=="Smooth")$mu, var.equal = TRUE)
cohensD(subset(dcond, cond=="Rough")$mu, subset(dcond, cond=="Smooth")$mu)
ttestBF(subset(dcond, cond=="Rough")$mu, subset(dcond, cond=="Smooth")$mu)

#test age
dage<-ddply(subset(dat,trial>0), ~id+agegroup,summarize, mu=mean(z))

#adults vs. older children
t.test(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu, var.equal = TRUE)
cohensD(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu)
ttestBF(subset(dage, agegroup==">18")$mu, subset(dage, agegroup=="9-11")$mu)

#older children vs. adults
t.test(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu, var.equal = TRUE)
cohensD(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu)
ttestBF(subset(dage, agegroup=="9-11")$mu, subset(dage, agegroup=="7-8")$mu)

#Distance
dat$dist<-0
for (i in 2:nrow(dat)){
  #absolute difference between current and previous move
  dat$dist[i]<-abs(dat$x[i]-dat$x[i-1])+abs(dat$y[i]-dat$y[i-1])
}

#Distance by condition
ddist<-ddply(subset(dat,trial>0), ~id+cond,summarize, mu=mean(dist))
t.test(subset(ddist, cond=="Rough")$mu, subset(ddist, cond=="Smooth")$mu, var.equal = TRUE)
cohensD(subset(ddist, cond=="Rough")$mu, subset(ddist, cond=="Smooth")$mu)
ttestBF(subset(ddist, cond=="Rough")$mu, subset(ddist, cond=="Smooth")$mu)

#Distance by age
ddista<-ddply(subset(dat,trial>0), ~id+agegroup,summarize, mu=mean(dist))

#adults vs. older children
t.test(subset(ddista, agegroup==">18")$mu, subset(ddista, agegroup=="9-11")$mu, var.equal = TRUE)
cohensD(subset(ddista, agegroup==">18")$mu, subset(ddista, agegroup=="9-11")$mu)
ttestBF(subset(ddista, agegroup==">18")$mu, subset(ddista, agegroup=="9-11")$mu)

#older vs. younger children
t.test(subset(ddista, agegroup=="9-11")$mu, subset(ddista, agegroup=="7-8")$mu, var.equal = TRUE)
cohensD(subset(ddista, agegroup=="9-11")$mu, subset(ddista, agegroup=="7-8")$mu)
ttestBF(subset(ddista, agegroup=="9-11")$mu, subset(ddista, agegroup=="7-8")$mu)


#Learning curves assessed by Kendall's tau
dt<-ddply(dat, ~id+cond+agegroup, summarize, c=cor(trial, z, method="spearman"))

#different from 0?
t.test(dt$c)
ttestBF(dt$c)

#difference between conditions
t.test(subset(dt, cond=="Rough")$c, subset(dt, cond=="Smooth")$c, var.equal = TRUE)
cohensD(subset(dt, cond=="Rough")$c, subset(dt, cond=="Smooth")$c)
ttestBF(subset(dt, cond=="Rough")$c, subset(dt, cond=="Smooth")$c)

#Adults vs. older children
t.test(subset(dt, agegroup==">18")$c, subset(dt, agegroup=="9-11")$c, var.equal = TRUE)
cohensD(subset(dt, agegroup==">18")$c, subset(dt, agegroup=="9-11")$c)
ttestBF(subset(dt, agegroup==">18")$c, subset(dt, agegroup=="9-11")$c)

#Older children vs. younger children
t.test(subset(dt, agegroup=="7-8")$c, subset(dt, agegroup=="9-11")$c, var.equal = TRUE)
cohensD(subset(dt, agegroup=="7-8")$c, subset(dt, agegroup=="9-11")$c)
ttestBF(subset(dt, agegroup=="7-8")$c, subset(dt, agegroup=="9-11")$c)

dat$tile<-paste0(dat$x, dat$y)
dd<-ddply(dat, ~id+round+agegroup, summarize, l=length(unique(tile)))
dd<-ddply(dd, ~id+agegroup, summarize, mu=mean(l))
#adults vs. older children
t.test(subset(dd, agegroup==">18")$mu, subset(dd, agegroup=="9-11")$mu, var.equal = TRUE)
cohensD(subset(dd, agegroup==">18")$mu, subset(dd, agegroup=="9-11")$mu)
ttestBF(subset(dd, agegroup==">18")$mu, subset(dd, agegroup=="9-11")$mu)

#older vs. younger children
t.test(subset(dd, agegroup=="9-11")$mu, subset(dd, agegroup=="7-8")$mu, var.equal = TRUE)
cohensD(subset(dd, agegroup=="9-11")$mu, subset(dd, agegroup=="7-8")$mu)
ttestBF(subset(dd, agegroup=="9-11")$mu, subset(dd, agegroup=="7-8")$mu)

