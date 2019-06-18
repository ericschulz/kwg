#Number of participants best explained
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'gridExtra', 'ggjoy', 'BayesFactor')
lapply(packages, require, character.only = TRUE)

#model data
gm<-read.csv("rbfgreedymean.csv")
gv<-read.csv("rbfgreedyvariance.csv")
gu<-read.csv("rbfucb.csv")
mm<-read.csv("mtgreedymean.csv")
mv<-read.csv("mtgreedyvariance.csv")
mu<-read.csv("mtucb.csv")

#combined frame
dd<-data.frame(gm=gm$r2,gv=gv$r2, gu=gu$r2, mm=mm$r2, mv=mv$r2, mu=mu$r2, id=mu$id)
dd<-ddply(dall, ~id+age, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))

dd$ucount<-apply(dd[,2:7], 1, which.max)==3
sum(dd$ucount)

#function to count number of best explained
mybayesfac<-function(x){
  bfInterval<- ttestBF(x= x, nullInterval=c(-Inf,0))
  out<- as.numeric(as.vector(bfInterval[2] / bfInterval[1]))
  return(out)
}


#initialize matrix
bf<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      bf[k]<-NA
    }
    if (i!=j){ 
      bf[k] <- mybayesfac(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}

bf
dp<-data.frame(this=this, that=that, bf=bf)

dat<-read.csv("kwgdata.csv")
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)
dat<-ddply(dat, ~id, summarise, age=agegroup[1], cond=cond[1])
dat$id<-1:160
#new frame
dall<-data.frame(gm=gm$r2,gv=gv$r2, gu=gu$r2, mm=mm$r2, mv=mv$r2, mu=mu$r2, id=mu$id)
dall$ucount<-dd$ucount
#intitialize condition
dall$condition<-0
#initialize age
dall$age<-0
#add age and condition
for (i in 1:nrow(dall)){
  dall$condition[i]<-dat$cond[dall$id[i]==dat$id]
  dall$age[i]<-dat$age[dall$id[i]==dat$id]
}

#Age 7-8
dd<-ddply(subset(dall, age=="7-8"), ~id, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))
dd$ucount<-apply(dd[,2:7], 1, which.max)==3
sum(dd$ucount)
nrow(dd)
#initialize matrix
bf<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      bf[k]<-NA
    }
    if (i!=j){ 
      bf[k] <- mybayesfac(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}
dp1<-data.frame(this=this, that=that, bf=bf)

dd<-ddply(subset(dall, age=="9-11"), ~id, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))
dd$ucount<-apply(dd[,2:7], 1, which.max)==3
sum(dd$ucount)
nrow(dd)
#initialize matrix
bf<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      bf[k]<-NA
    }
    if (i!=j){ 
      bf[k] <- mybayesfac(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}
dp2<-data.frame(this=this, that=that, bf=bf)

dd<-ddply(subset(dall, age==">18"), ~id, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))
dd$ucount<-apply(dd[,2:7], 1, which.max)==3
sum(dd$ucount)
nrow(dd)
#initialize matrix
bf<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      bf[k]<-NA
    }
    if (i!=j){ 
      bf[k] <- mybayesfac(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}

dp3<-data.frame(this=this, that=that, bf=bf)

dplot<-rbind(dp, dp1, dp2, dp3)
nrow(dat)
table(dat$age)
dplot$this<-factor(dplot$this, levels=names)
dplot$that<-factor(dplot$that, levels=rev(names))
dplot$age<-rep(c("Overall", "7-8", "9-11", ">18"), each=36)
dplot$age<-factor(dplot$age, 
                  levels=c("Overall", "7-8", "9-11", ">18"))
dplot$b<-dplot$bf/ave(dplot$bf, dplot$age, FUN=function(x){max(x, na.rm=TRUE)})

dplot$an<-ifelse(is.na(round(log10(dplot$bf))), "", paste(round(log10(dplot$bf))))
dplot$bf<-round(log10(dplot$bf))
pl1 <-  ggplot(dplot, aes(y = this,  x = that)) +  
  #geom_tile(aes(fill = "white")) +  
  xlab("Compared against")+ylab("Model")+
  scale_fill_continuous(low = "blue", high = "green") +
  geom_point(aes(colour = b,  size =b)) +
  facet_wrap(~age,nrow=2, scales="free")+
  geom_text(aes(that, this, label=an))+
  scale_color_gradient(low = "yellow",   high = "red") +
  scale_size(range = c(2, 14))+   theme_minimal()+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position="none")+ggtitle("Log-scaled Bayes Factor")

pdf("bayesfactor.pdf", width=7.5, height=7.5)
pl1
dev.off()
getwd()
