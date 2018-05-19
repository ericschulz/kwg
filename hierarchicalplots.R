rm(list=ls())

library(ggplot2)
library(plyr)
library(gridExtra)

dm<-read.csv("learning.csv")
dat<-read.csv("kwgdata.csv")

dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)
dat<-ddply(dat, ~id, summarise, agegroup=agegroup[1])
fontsize<-16
#First plot, intercept
p1<-ggplot(data.frame(x = c(15, 50)), aes(x)) + 
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = "black", alpha=0.1)
  }, 
  # enter means, standard deviations and colors here
  mean = dm$mean[4:163], 
  sd = dm$sd[4:163]
  )+
  stat_function(fun = dnorm, args = list(mean = dm$mean[1], sd = dm$sd[1]), col = "red")+
  theme_minimal()+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  ggtitle("Mean")+xlab(expression(theta[0]))+
  theme(text = element_text(size=fontsize,  family="sans"))+
  ylab("Density")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

p1

#Second plot, trials
p2<-ggplot(data.frame(x = c(-5, 10)), aes(x)) + 
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = "black", alpha=0.1)
  }, 
  # enter means, standard deviations and colors here
  mean = dm$mean[324:483], 
  sd = dm$sd[324:483])+
  stat_function(fun = dnorm, args = list(mean = dm$mean[3], sd = dm$sd[3]), col = "red")+
  theme_minimal()+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  theme(text = element_text(size=fontsize,  family="sans"))+
  ggtitle("Trials")+xlab(expression(theta[1]))+
  ylab("Density")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

p2

#Third plot, rounds
p3<-ggplot(data.frame(x = c(-12, 12)), aes(x)) + 
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = "black", alpha=0.1)
  }, 
  # enter means, standard deviations and colors here
  mean = dm$mean[164:323], 
  sd = dm$sd[164:323])+
  stat_function(fun = dnorm, args = list(mean = dm$mean[2], sd = dm$sd[2]), col = "red")+
  theme_minimal()+
  theme(text = element_text(size=fontsize,  family="sans"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  ggtitle("Rounds")+xlab(expression(theta[2]))+
  ylab("Density")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

p3

#save them all
pdf("trialsandrounds.pdf", width=12, height=4)
grid.arrange(p1,p2,p3, nrow=1)
dev.off()
