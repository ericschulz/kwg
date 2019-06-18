#Number of participants best explained
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'gridExtra', 'ggjoy')
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
dd<-ddply(dd, ~id, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))

#function to count number of best explained
countbetter<-function(x){
  return(sum(x>0))
}

propbetter<-function(x){
  return(sum(x>0)/length(x))
}

#initialize matrix
count<-prop<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      count[k]<-NA
      prop[k]<-NA
    }
    if (i!=j){ 
      count[k] <- countbetter(dd[,i+1]-dd[,j+1]) 
      prop[k] <- propbetter(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}

dp<-data.frame(this=this, that=that, count=count, prop=prop)

dat<-read.csv("kwgdata.csv")
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)
dat<-ddply(dat, ~id, summarise, age=agegroup[1], cond=cond[1])
dat$id<-1:160
#new frame
dall<-data.frame(gm=gm$r2,gv=gv$r2, gu=gu$r2, mm=mm$r2, mv=mv$r2, mu=mu$r2, id=mu$id)

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
#initialize matrix
count<-prop<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      count[k]<-NA
      prop[k]<-NA
    }
    if (i!=j){ 
      count[k] <- countbetter(dd[,i+1]-dd[,j+1]) 
      prop[k] <- propbetter(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}
dp1<-data.frame(this=this, that=that, count=count, prop=prop)

dd<-ddply(subset(dall, age=="9-11"), ~id, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))
#initialize matrix
count<-prop<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      count[k]<-NA
      prop[k]<-NA
    }
    if (i!=j){ 
      count[k] <- countbetter(dd[,i+1]-dd[,j+1]) 
      prop[k] <- propbetter(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}
dp2<-data.frame(this=this, that=that, count=count, prop=prop)

dd<-ddply(subset(dall, age==">18"), ~id, summarize, gm=mean(gm), gv=mean(gv), gu=mean(gu), mm=mean(mm), mv=mean(mv), mu=mean(mu))
#initialize matrix
count<-prop<-this<-that<-rep(0,36)
#loop through
k<-1
names<-c("GP-M", "GP-V", "GP-UCB","MT-M", "MT-V", "MT-UCB" )
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      count[k]<-NA
      prop[k]<-NA
    }
    if (i!=j){ 
      count[k] <- countbetter(dd[,i+1]-dd[,j+1]) 
      prop[k] <- propbetter(dd[,i+1]-dd[,j+1]) 
    }
    this[k]<-names[i]
    that[k]<-names[j]
    k<-k+1
  }
}
dp3<-data.frame(this=this, that=that, count=count, prop=prop)

dplot<-rbind(dp, dp1, dp2, dp3)
nrow(dat)
table(dat$age)
dplot$this<-factor(dplot$this, levels=names)
dplot$that<-factor(dplot$that, levels=rev(names))
dplot$age<-rep(c("Across all age groups (N=160)", "Age 7-8 (N=55)", "Age 9-11 (N=55)", "Age >18 (N=50)"), each=36)
dplot$age<-factor(dplot$age, 
                  levels=c("Across all age groups (N=160)", "Age 7-8 (N=55)", "Age 9-11 (N=55)", "Age >18 (N=50)"))
dplot$an<-ifelse(is.na(dplot$count), "", paste(dplot$count))
pl1 <-  ggplot(dplot, aes(y = this,  x = that)) +  
  #geom_tile(aes(fill = "white")) +  
  xlab("Compared against")+ylab("Model")+
  scale_fill_continuous(low = "blue", high = "green") +
  geom_point(aes(colour = prop,  size =prop)) +
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
        legend.position="none")+ggtitle("Number of participants best predicted")

pdf("bestdescribed.pdf", width=7.5, height=7.5)
pl1
dev.off()