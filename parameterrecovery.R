de<-read.csv("recoveredgp.csv")
dt<-read.csv("gpucbparams.csv")

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


de$lambda<-exp(de$par1)
de$beta<-exp(de$par2)
de$tau<-exp(de$par3)
library(MASS)
library(plyr)
library(ggplot2)
mymean<-function (x){mean(x[x<=5])}

de$id<-rep(1:160, each=8)
de<-ddply(de, ~id,summarize, lambda=mymean(lambda), beta=mymean(beta), tau=mymean(tau))
cor.test(de$tau, dt$tau, method = "kendall")
kendallci(de$tau, dt$tau)
cor.test(de$beta, dt$beta, method = "kendall")
kendallci(de$beta, dt$beta)
cor.test(de$lambda, dt$lambda, method="kendall")
kendallci(de$lambda, dt$lambda)


betaCor <- cor.test(de$beta, dt$beta, method='kendall') #correlation test

axisScale <- function(vec1, vec2){ #Finds the larger of the two axis limits that omits outliers more than 1.5IQR from the 75th percentile
  max1 <- min(max(vec1), median(vec1, 0.75) + 1.5 * IQR(vec1))
  max2 <- min(max(vec2), quantile(vec2, 0.75) + 1.5 * IQR(vec2))
  return(max(max1, max2)+0.1)
}



cor2D<-data.frame(lambda1=dt$lambda, lambda2=de$lambda)
cor2D$lambdadensity <- fields::interp.surface(MASS::kde2d(de$lambda, dt$lambda), cor2D[,c("lambda1", "lambda2")]) #bivariate point density


p1 <- ggplot(cor2D, aes(lambda1, lambda2, fill=lambdadensity, alpha = 1/lambdadensity)) +
  geom_point(pch=21, size =3, color='black')+
  geom_smooth(data=subset(cor2D, lambda1 <= axisScale(cor2D$lambda1, cor2D$lambda2) & lambda2 <= axisScale(cor2D$lambda1, cor2D$lambda2)), method='lm', color='black', se=FALSE, linetype=2, fullrange=TRUE)+
  scale_fill_gradient(low = "#0091ff", high = "#f0650e") +
  scale_alpha(range = c(.4, 1)) + guides(alpha="none", fill="none") +
  scale_x_continuous(expand=c(0,0), limits=c(0,axisScale(cor2D$lambda1, cor2D$lambda2))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,axisScale(cor2D$lambda1, cor2D$lambda2))) +
  xlab(expression(lambda[1]))+
  ylab(expression(lambda[2])) +
  coord_cartesian(xlim=c(0,axisScale(cor2D$lambda1, cor2D$lambda2)), ylim=c(0,axisScale(cor2D$lambda1, cor2D$lambda2)))+
  annotate("text", x = 0.3, y = 1.15, label = "r[tau] == '.85'", parse=TRUE, size=6)+
  theme(title = element_text(family = 'serif'))+
  theme_minimal()+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position="none")+ggtitle(expression("Generalization parameter"~lambda))
p1


cor2D<-data.frame(beta1=dt$beta, beta2=de$beta)
cor2D$betadensity <- fields::interp.surface(MASS::kde2d(de$beta, dt$beta), cor2D[,c("beta1", "beta2")]) #bivariate point density


p2 <- ggplot(cor2D, aes(beta1, beta2, fill=betadensity, alpha = 1/betadensity)) +
  geom_point(pch=21, size =3, color='black')+
  geom_smooth(data=subset(cor2D, beta1 <= axisScale(cor2D$beta1, cor2D$beta2) & beta2 <= axisScale(cor2D$beta1, cor2D$beta2)), method='lm', color='black', se=FALSE, linetype=2, fullrange=TRUE)+
  scale_fill_gradient(low = "#0091ff", high = "#f0650e") +
  scale_alpha(range = c(.4, 1)) + guides(alpha="none", fill="none") +
  scale_x_continuous(expand=c(0,0), limits=c(0,axisScale(cor2D$beta1, cor2D$beta2))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,axisScale(cor2D$beta1, cor2D$beta2))) +
  xlab(expression(beta[1]))+
  ylab(expression(beta[2])) +
  coord_cartesian(xlim=c(0,axisScale(cor2D$beta1, cor2D$beta2)), ylim=c(0,axisScale(cor2D$beta1, cor2D$beta2)))+
  annotate("text", x = 0.2, y = .65, label = "r[tau] == '.75'", parse=TRUE, size=6)+
  theme(title = element_text(family = 'serif'))+
  theme_minimal()+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position="none")+ggtitle(expression("Exploration parameter"~beta))
p2


cor2D<-data.frame(tau1=dt$tau, tau2=de$tau)
cor2D$taudensity <- fields::interp.surface(MASS::kde2d(de$tau, dt$tau), cor2D[,c("tau1", "tau2")]) #bivariate point density


p3 <- ggplot(cor2D, aes(tau1, tau2, fill=taudensity, alpha = 1/taudensity)) +
  geom_point(pch=21, size =3, color='black')+
  geom_smooth(data=subset(cor2D, tau1 <= axisScale(cor2D$tau1, cor2D$tau2) & tau2 <= axisScale(cor2D$tau1, cor2D$tau2)), method='lm', color='black', se=FALSE, linetype=2, fullrange=TRUE)+
  scale_fill_gradient(low = "#0091ff", high = "#f0650e") +
  scale_alpha(range = c(.4, 1)) + guides(alpha="none", fill="none") +
  scale_x_continuous(expand=c(0,0), limits=c(0,axisScale(cor2D$tau1, cor2D$tau2))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,axisScale(cor2D$tau1, cor2D$tau2))) +
  xlab(expression(tau[1]))+
  ylab(expression(tau[2])) +
  coord_cartesian(xlim=c(0,axisScale(cor2D$tau1, cor2D$tau2)), ylim=c(0,axisScale(cor2D$tau1, cor2D$tau2)))+
  annotate("text", x = 0.05, y = .137, label = "r[tau] == '.81'", parse=TRUE, size=6)+
  theme(title = element_text(family = 'serif'))+
  theme_minimal()+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position="none")+ggtitle(expression("Temperature parameter"~tau))
p3

library(gridExtra)

pdf("recovery.pdf", width=14/1.6, height=5/1.6)
grid.arrange(p1,p2,p3, nrow=1)
dev.off()

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
de$condition<-0
#initialize age
de$age<-0
#add age and condition
for (i in 1:nrow(de)){
  de$condition[i]<-dat$cond[de$id[i]==dat$id]
  de$age[i]<-dat$age[de$id[i]==dat$id]
}

#Source Mann-Whitney-U-BF
source("mannwhitbf.R")


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
#mean per id and age
dage<-ddply(de, ~id+age,summarize, mu=mean(beta))
#Mann-Whitney-U test for adults vs. old kids
wilcox.test(subset(dage, age==">18")$mu, subset(dage, age=="9-11")$mu)
#subset of data frame
dd<-subset(dage, age %in% c(">18", "9-11"))
#rank correlation as effect size
cor(ifelse(dd$age==">18",1,0), dd$mu, method="kendall")
kendallci(ifelse(dd$age==">18",1,0), dd$mu)
#get Bayes Factor
outsim<-rankSumGibbsSampler(subset(dd, age==">18")$mu, subset(dd, age=="9-11")$mu)
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
kendallci(ifelse(dd$age=="9-11",1,0), dd$mu)
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
#ANOVA is impossible so, we report the biggest BF, which was for 9-11 vs adults
outsim<-rankSumGibbsSampler(subset(dage, age!=">18")$mu, subset(dage, age==">18")$mu)
dense<- density(outsim$deltaSamples)
ddense <- with(dense, approxfun(x, y, rule=1))
BF<-dcauchy(0, location = 0, scale = 1/sqrt(2), log = FALSE)/ddense(0)
print(BF)
