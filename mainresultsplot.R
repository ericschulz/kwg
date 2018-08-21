#Behvavioral plots
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'gridExtra', 'ggjoy', "tikzDevice")
lapply(packages, require, character.only = TRUE)

#read in data
myjson<-fromJSON("kwg.json")

#empty frame
dat<-data.frame(id=numeric(), cond=numeric(), age=numeric(),
                x=numeric(), y=numeric(), z=numeric(), time=numeric(),
                trial=numeric(), round=numeric())


#loop through participants
for (i in 1:myjson$count){
  
  #we need rounds 2-9
  x<-as.vector(t(myjson$records$data$searchHistory$xcollect[[i]][2:9,]))
  y<-as.vector(t(myjson$records$data$searchHistory$ycollect[[i]][2:9,]))
  z<-as.vector(t(myjson$records$data$searchHistory$zcollect[[i]][2:9,]))
  time<-as.vector(t(myjson$records$data$searchHistory$tscollect[[i]][2:9,]))
  cond<-rep(myjson$records$data$condition[i], length(x))
  age<-rep(myjson$records$data$age[i], length(x))
  trial<-rep(0:25, 8)
  round<-rep(1:8, each=26)
  id<-rep(i, length(x))
  #get the dummy frame
  dummy<-data.frame(id=id, cond=cond, age=age, x=x, y=y, z=z, time=time, trial=trial, round=round)
  #conctatenate
  dat<-rbind(dat, dummy)
}

#first 10 entries are us!
dat<-subset(dat, id>10)

#kids younger than 7 are not allowed
dat<-subset(dat, age>=7)

#fontsize is 8
fontsize<-14
#standard error
se<-function(x){sd(x)/sqrt(length(x))}

#age groups are 7-8, 9-11, and adults
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)

#conditions are smooth and rough
dat$Condition<-ifelse(dat$cond==1, "Rough", "Smooth")


#bootstrapped upper CI
upci<-function(x, id){
  x<-ddply(data.frame(id,x), ~id, summarize, x=mean(x))$x
  return(mean(x)+2.14*(sd(x)/sqrt(length(x))))
}

#bootstrapped lower ci
downci<-function(x, id){
  x<-ddply(data.frame(id,x), ~id, summarize, x=mean(x))$x
  mean(x)-2.14*(sd(x)/sqrt(length(x)))
}

dat$Age<-dat$agegroup
#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(subset(dat,trial>0), ~Condition+Age, summarize,
           d_ymin = max(min(z), quantile(z, 0.25) - 1.5 * IQR(z)), 
           d_ymax = min(max(z), quantile(z, 0.75) + 1.5 * IQR(z)),
           d_lower = quantile(z, 0.25),  d_middle = median(z), d_upper = quantile(z, 0.75),
           mu=mean(z))



cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


########################################################
#Figure A: Error
########################################################
dp1$Age<-factor(dp1$Age, levels=c("7-8", "9-11", ">18"))
dat$Age<-factor(dat$Age, levels=c("7-8", "9-11", ">18"))
dat1<-ddply(dat, ~id+Age+Condition, summarize, z=mean(z))

p1<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dat1, aes(x = as.numeric(Age) + 0.2,  y = z,  color = Age),
              width = 0.2 - 0.25 * 0.2, height = 0, size=1.2, alpha=0.5)+
  #vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  #geom_segment(aes(x = as.numeric(Age)+0.4, y = cidown, xend = as.numeric(Age)+0.4, yend = ciup), size = 2, color="grey30") +
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +facet_wrap(~Condition)+
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=fontsize,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #labs
  xlab("Age")+ylab("Reward")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8","9-11", ">18"))+ggtitle("A: Performance")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

p1

#distance of previous sample to current
dat$dist<-0
#loop through
for (i in 2:nrow(dat)){
  #absolute distance
  dat$dist[i]<-abs(dat$x[i]-dat$x[i-1])+abs(dat$y[i]-dat$y[i-1])
}

#SECOND PLOT: 
p2<-ggplot(subset(dat,dist<=10 & trial !=0), aes(x = dist, y = Age, fill=Age, height = ..density..)) +
  #joy plot but not as density but as bins
  geom_joy(alpha = .7, stat = "binline", bins = 10.5, scale = 0.9)+theme_minimal()+
  #color code
  scale_fill_manual(values = cbPalette[c(7,6,1)])+
  #wrap by condition
  facet_wrap(~Condition)+
  #labs
  ylab("Age")+xlab("Distance")+
  #0 to 10
  scale_x_continuous(breaks = seq(0,10,2))+
  #title
  ggtitle("B: Search behavior")+
  #expand
  scale_y_discrete(expand = c(0, 0)) +
  #theme fontsize
  theme(legend.position="none", strip.background=element_blank(),
        text = element_text(size=fontsize,  family="sans"))+
  #line marking repeats
  geom_vline(xintercept=0.5, size=1.15, col="red")+
  geom_hline(yintercept = 10, size=1)+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p2

pd <- position_dodge(.1)
#summarize by agegroup, trial, and condition
dd<-ddply(dat, ~agegroup+trial+cond, summarize, mu=mean(z), se=se(z))
#rough and smooth
dd$Cond<-ifelse(dd$cond==1, "Rough", "Smooth")
dd$Cond<-factor(dd$Cond, levels = c("Smooth", "Rough"))
#call it Age again
dd$Age<-dd$agegroup
#factors...
dd$Age<-factor(dd$Age, levels=c("7-8", "9-11", ">18"))


#THIRD PLOT
##Learning over trials by age and condition
p3<-ggplot(dd, aes(x=trial, y=mu, group=interaction(Cond, Age), col=Age, linetype=Cond)) +
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
  theme(legend.position ="top", 
        strip.background=element_blank(), 
        legend.key=element_rect(color=NA))+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-15,-10))
p3


#NOW COMES THE MODELLING RESULTS

#RBF results
drbf<-read.csv("rbf.csv")
#Mean Tracker results
dbmt<-read.csv("bmt.csv")
#pseudo-r^2
drbf$r2<-(1-drbf$X.2/(-25*log(1/64)))
dbmt$r2<-(1-dbmt$X.2/(-25*log(1/64)))


#this is to match the IDs as they'd been transformed to `:160
dat<-ddply(dat, ~id, summarize, age=agegroup[1], cond=Condition[1])
dat$id<-1:160

#model names
drbf$model<-"RBF"
dbmt$model<-"BMT"

#combine them
d<-data.frame(r2=c(drbf$r2, dbmt$r2), model=c(drbf$model, dbmt$model), id=c(drbf$id, dbmt$id))

#match the conditions
d$condition<-0
d$age<-0
dat$age
for (i in 1:nrow(d)){
  d$condition[i]<-dat$cond[d$id[i]==dat$id]
  d$age[i]<-dat$age[d$id[i]==dat$id]
  
}

#age variable
d$Age<-d$age

#factors
d$Age<-factor(d$Age, levels=c("7-8", "9-11", ">18"))

pd <- position_dodge(.1)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp4<-ddply(d, ~model+Age, summarize, 
           d_ymin = max(min(r2), quantile(r2, 0.25) - 1.5 * IQR(r2)), 
           d_ymax = min(max(r2), quantile(r2, 0.75) + 1.5 * IQR(r2)),
           d_lower = quantile(r2, 0.25),  d_middle = median(r2), d_upper = quantile(r2, 0.75),
           mu=mean(r2))

head(d)
d1<-ddply(d, ~id+Age+condition+model, summarize, r2=mean(r2))

p4<-ggplot(data = dp4) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(model)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=d1, aes(x = as.numeric(model) + 0.2,  y = r2,  color = Age), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(model), y = d_ymin, xend = as.numeric(model), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(model)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(model) - 0.1,  y = d_ymax, xend = as.numeric(model),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(model) - 0.1, y = d_ymin, xend = as.numeric(model), yend = d_ymin)) +facet_wrap(~Age)+
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=fontsize,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #labs
  xlab("Model")+ylab(expression("Predictive accuracy:"~R^2))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2),labels = c("MT","GP"))+ggtitle("D: Model comparison")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p4


#parameters
dp<-data.frame(estimate=exp(c(drbf$par1, drbf$par2,drbf$par3)), id=rep(drbf$id, 3), param=rep(c("lambda", "beta", "tau"), each=nrow(drbf)))
sum(subset(dp, param=="beta")$estimate>5)/nrow(subset(dp, param=="beta"))
sum(subset(dp, param=="tau")$estimate>5)/nrow(subset(dp, param=="tau"))
sum(subset(dp, param=="lambda")$estimate>5)/nrow(subset(dp, param=="lambda"))
#condition and age
dp$condition<-0
dp$age<-0

for (i in 1:nrow(dp)){
  dp$condition[i]<-dat$cond[dp$id[i]==dat$id]
  dp$age[i]<-dat$age[dp$id[i]==dat$id]
}

dp$Age<-dp$age
dp$Age<-factor(dp$Age, levels=c("7-8", "9-11", ">18"))
dp<-ddply(dp, ~id+param+Age, summarize, estimate=median(estimate[estimate<=5]))
dp<-na.omit(dp)
dp$param<- factor(dp$param,levels=c("lambda", "beta", "tau"), 
                  labels = c(expression("Generalization"~lambda),
                             expression("Exploration"~beta), 
                             expression("Temperature"~tau)))
#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp5<-ddply(dp, ~param+Age, summarize,
           d_ymin = max(min(estimate), quantile(estimate, 0.25) - 1.5 * IQR(estimate)), 
           d_ymax = min(max(estimate), quantile(estimate, 0.75) + 1.5 * IQR(estimate)),
           d_lower = quantile(estimate, 0.25),  d_middle = median(estimate), d_upper = quantile(estimate, 0.75),
           mu=mean(estimate))


p5<-ggplot(data = dp5) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dp, aes(x = as.numeric(Age) + 0.2,  y = estimate,  color = Age), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +
  facet_wrap(~param, labeller = label_parsed)+
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=fontsize,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #labs
  xlab("Age")+ylab("Estimate")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  coord_cartesian(ylim = c(0,2))+
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8","9-11", ">18"))+ggtitle("E: Parameter estimates")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+ylim(c(0,2))
p5

#getting the learning curves in
dcollect<-read.csv("learningcurves.csv")
#add age groups
dcollect$agegroup<-ifelse(dcollect$age<9, "7-8", dcollect$age)
dcollect$agegroup<-ifelse(dcollect$age>=9 & dcollect$age <12, "9-11", dcollect$agegroup)
dcollect$agegroup<-ifelse(dcollect$age>18, ">18", dcollect$agegroup)
#add condition
dcollect$Condition<-ifelse(dcollect$cond==1, "Rough", "Smooth")

#get means over trials
dd<-ddply(dcollect, ~agegroup+trial+Condition, summarize, mu=mean(z), se=se(z))
#factors
dd$Cond<-factor(dd$Condition, levels = c("Smooth", "Rough"))
dd$Age<-dd$agegroup
dd$Age<-factor(dd$Age, levels=c("7-8", "9-11", ">18"))

#dodge
pd <- position_dodge(.1)
#aligning correctly
dd$mu<-dd$mu+3
dd$const<-""

#LAST PLOT
##learning curves

p6<-ggplot(dd, aes(x=trial, y=mu, group=interaction(Cond, Age), col=Age, linetype=Cond)) +
  #means over time
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
  scale_color_manual(values = cbPalette[c(7,6,1)])+
  theme(text = element_text(size=fontsize,  family="sans")) +
  #title
  ggtitle("F: Simulated learning curves")+
  theme(legend.position ="top", 
        strip.background=element_blank(), 
        legend.key=element_rect(color=NA))+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-15,-10))
p6
#create tikz of all plots in a grid
pdf(file = "bigplot.pdf", width = 14, height = 7.5)
grid.arrange(p1,p2, p3, p4, p5, p6, nrow=2)
dev.off()
