#Full MT-model plots
#Eric Schulz, March 2018

#house keeping
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'gridExtra', 'ggjoy', "tikzDevice")
lapply(packages, require, character.only = TRUE)

dm<-read.csv("mtgreedymean.csv")
dv<-read.csv("mtgreedyvariance.csv")
du<-read.csv("mtucb.csv")

dall<-rbind(dm, dv, du)
dall$model<-rep(c("Exploitation", "Exploration", "UCB (both)"), each=nrow(dm))


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


#match the conditions
dall$condition<-0
dall$age<-0
for (i in 1:nrow(dall)){
  dall$condition[i]<-dat$cond[dall$id[i]==dat$id]
  dall$age[i]<-dat$age[dall$id[i]==dat$id]
}

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

dall$Age<-dall$age
dall$Condition<-dall$condition
dall$Age<-factor(dall$Age, levels=c("7-8","9-11", ">18"))
dall$Condition<-factor(dall$Condition, levels=c("Rough", "Smooth"))

dall$model<-factor(dall$model, levels=c("Exploitation", "Exploration", "UCB (both)"))

#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(dall, ~Condition+Age+model, summarize, 
           d_ymin = max(min(r2), quantile(r2, 0.25) - 1.5 * IQR(r2)), 
           d_ymax = min(max(r2), quantile(r2, 0.75) + 1.5 * IQR(r2)),
           d_lower = quantile(r2, 0.25),  d_middle = median(r2), d_upper = quantile(r2, 0.75),
           mu=mean(r2))

fontsize<-17
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pall<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(model)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dall, aes(x = as.numeric(model) + 0.2,  y = r2,  color = Age), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(model), y = d_ymin, xend = as.numeric(model), yend = d_ymax)) +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(model) - 0.1,  y = d_ymax, xend = as.numeric(model),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(model) - 0.1, y = d_ymin, xend = as.numeric(model), yend = d_ymin)) +facet_grid(Condition~Age)+
  geom_point(aes(x = as.numeric(model)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=fontsize,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #labs
  xlab("Sampling strategy")+ylab(expression("Predicitive accuracy:"~r^2))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2,3),labels = c(expression(M(x)),expression(V(x)), expression(UCB(x))))+
  ggtitle("A: MT predictive performance")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
pall

#parameters
dp<-data.frame(estimate=exp(c(du$par1, du$par2,du$par3)), id=rep(du$id, 3), param=rep(c("theta[epsilon]^2", "beta", "tau"), each=nrow(du)))
dp<-subset(dp, estimate<3)
#condition and age
dp$condition<-0
dp$age<-0

for (i in 1:nrow(dp)){
  dp$condition[i]<-dat$cond[dp$id[i]==dat$id]
  dp$age[i]<-dat$age[dp$id[i]==dat$id]
}

dp$Age<-dp$age
dp$Age<-factor(dp$Age, levels=c("7-8", "9-11", ">18"))
dp$Condition<-factor(dp$condition, levels=c("Rough", "Smooth"))
dp$param<-factor(dp$param, levels=(c(expression(theta[epsilon]^2), expression(beta), expression(tau))))
#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean

dp2<-ddply(dp, ~Condition+Age+param, summarize,
           d_ymin = max(min(estimate), quantile(estimate, 0.25) - 1.5 * IQR(estimate)), 
           d_ymax = min(max(estimate), quantile(estimate, 0.75) + 1.5 * IQR(estimate)),
           d_lower = quantile(estimate, 0.25),  d_middle = median(estimate), d_upper = quantile(estimate, 0.75),
           mu=mean(estimate))


p2<-ggplot(data = dp2) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dp, aes(x = as.numeric(Age) + 0.2,  y = estimate,  color = Age), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +
  facet_grid(Condition~param,  labeller = label_parsed)+  #theme minimal
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
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8", "9-11", ">18"))+
  ggtitle("B: MT-UCB parameter estimates")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p2


dp<-data.frame(estimate=exp(c(dm$par1, dm$par3)), id=rep(dm$id, 2), param=rep(c("theta[epsilon]^2", "tau"), each=nrow(dm)))
#condition and age
dp$condition<-0
dp$age<-0

for (i in 1:nrow(dp)){
  dp$condition[i]<-dat$cond[dp$id[i]==dat$id]
  dp$age[i]<-dat$age[dp$id[i]==dat$id]
}

dp$Age<-dp$age
dp$Age<-factor(dp$Age, levels=c("7-8", "9-11", ">18"))
dp$Condition<-factor(dp$condition, levels=c("Rough", "Smooth"))
dp$param<-factor(dp$param, levels=(c("theta[epsilon]^2", "tau")))
dp<-subset(dp, estimate<3)

#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp3<-ddply(dp, ~Condition+Age+param, summarize,
           d_ymin = max(min(estimate), quantile(estimate, 0.25) - 1.5 * IQR(estimate)), 
           d_ymax = min(max(estimate), quantile(estimate, 0.75) + 1.5 * IQR(estimate)),
           d_lower = quantile(estimate, 0.25),  d_middle = median(estimate), d_upper = quantile(estimate, 0.75),
           mu=mean(estimate))

p3<-ggplot(data = dp3) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dp, aes(x = as.numeric(Age) + 0.2,  y = estimate,  color = Age), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +
  facet_grid(Condition~param,  labeller = label_parsed)+  #theme minimal
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
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8", "9-11", ">18"))+
  ggtitle("C: MT-Mean greedy parameter estimates")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p3



dp<-data.frame(estimate=exp(c(dv$par1, dv$par3)), id=rep(dv$id, 2), param=rep(c("theta[epsilon]^2", "tau"), each=nrow(dv)))
#condition and age
dp$condition<-0
dp$age<-0

for (i in 1:nrow(dp)){
  dp$condition[i]<-dat$cond[dp$id[i]==dat$id]
  dp$age[i]<-dat$age[dp$id[i]==dat$id]
}

dp$Age<-dp$age
dp$Age<-factor(dp$Age, levels=c("7-8", "9-11", ">18"))
dp$Condition<-factor(dp$condition, levels=c("Rough", "Smooth"))
dp$param<-factor(dp$param, levels=(c("theta[epsilon]^2", "tau")))
dp<-subset(dp, estimate<3)

#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp4<-ddply(dp, ~Condition+Age+param, summarize, 
           d_ymin = max(min(estimate), quantile(estimate, 0.25) - 1.5 * IQR(estimate)), 
           d_ymax = min(max(estimate), quantile(estimate, 0.75) + 1.5 * IQR(estimate)),
           d_lower = quantile(estimate, 0.25),  d_middle = median(estimate), d_upper = quantile(estimate, 0.75),
           mu=mean(estimate))


p4<-ggplot(data = dp4) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dp, aes(x = as.numeric(Age) + 0.2,  y = estimate,  color = Age), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +
  facet_grid(Condition~param, labeller = label_parsed)+  #theme minimal
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
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8", "9-11", ">18"))+
  ggtitle("D: MT-Variance greedy parameter estimates")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p4

#create pdf of all plots in a grid
pdf(file = "fullmt.pdf", width = 16, height = 16)
grid.arrange(pall,p2, p3, p4, nrow=2)
dev.off()

