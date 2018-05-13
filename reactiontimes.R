#Reaction time plots
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


dat$Age<-dat$agegroup
dat$time<-c(0, diff(dat$time))

100-100*nrow(subset(dat,trial>0 & time <5000 & time >100))/nrow(subset(dat,trial>0))

#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(subset(dat,trial>0 & time <5000 & time >100), ~Condition+Age, summarize, 
           d_ymin = max(min(log(time)), quantile(log(time), 0.25) - 1.5 * IQR(log(time))), 
           d_ymax = min(max(log(time)), quantile(log(time), 0.75) + 1.5 * IQR(log(time))),
           d_lower = quantile(log(time), 0.25),  d_middle = median(log(time)), d_upper = quantile(log(time), 0.75),
           mu=mean(log(time)))
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


########################################################
#Figure A: Error
########################################################
dp1$Age<-factor(dp1$Age, levels=c("7-8", "9-11", ">18"))
dat$Age<-factor(dat$Age, levels=c("7-8", "9-11", ">18"))

p1<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=subset(dat,trial>0  & time <5000 & time >100), aes(x = as.numeric(Age) + 0.2,  y = (log(time)),  color = Age), alpha=0.1,
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
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
  xlab("Age")+ylab("Log-time")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8","9-11", ">18"))+ggtitle("A: Reaction times")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)))

p1


dat$prev<-round(c(0, dat$z[-length(dat$z)])/3)*3
p2 <- ggplot(subset(dat,prev>=0 & prev<=50 & trial>0 & time <5000), aes(x=prev, y = log(time), color = Age, fill=Age)) +
  #geom_count(alpha=0.2, show.legend = F, position = position_dodge(width=0.1))+
  #scale_size_area(max_size = 5)+
  #geom_jitter(alpha=0.05, size=0.5)+
  stat_summary(fun.y = mean, geom = 'line')+
  stat_summary(fun.data = mean_se, geom = 'ribbon', alpha = 0.5, color=NA) +
  ylab("Reaction time on t+1")+
  xlab('Reward on t')+ggtitle("B: Reaction time and rewards")+
  #ylim(c(0,50))+
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #scale_color_brewer(palette = 'Dark2', name="")+
  #scale_fill_brewer( palette = 'Dark2', name="")+
  facet_grid(~Condition)+
  theme_minimal()+
  theme(text = element_text(size=fontsize,  family="sans"))+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        legend.position = "none")

p2

library(gridExtra)
pdf("reactiontimes.pdf", width=10, height=4)
grid.arrange(p1,p2, nrow=1)
dev.off()
