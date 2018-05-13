#Judgement plots
#Eric Schulz, March 2018

#house keepinh
rm(list=ls())

#packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'boot', 'tikzDevice', 'gridExtra')
lapply(packages, require, character.only = TRUE)

#two environments
environments1 <- fromJSON("kernelRough.json")
environments2 <- fromJSON("kernelSmooth.json")

#data
myjson<-fromJSON("kwg.json")

#extract the two environments
roughEnvironments <- lapply(environments1, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1'))))
smoothEnvironments <- lapply(environments2, FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('x2', 'y', 'x1' ))))

#empty data frame
dat<-data.frame(id=numeric(), age=numeric(), x=numeric(), y=numeric(), est=numeric(), cer=numeric(), cond=numeric(), truth=numeric(), chosen=numeric())

#loop through json file
for (i in 1:myjson$count){
  
  #x-position
  x<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$x)
  #y-position
  y<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$y)
  #estimates
  est<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$givenValue)
  #certainty estimates
  cert<-as.numeric(myjson$records$data$bonusLevel$bonusCells[[i]]$howSecure)
  #environments
  env<-myjson$records$data$envOrder[[i]][10]
  #condition
  condition<-myjson$records$data$condition[i]
  #smooth or rough
  if(condition==0){ee<-data.frame(smoothEnvironments[[env+1]])}
  if(condition==1){ee<-data.frame(roughEnvironments[[env+1]])}
  #truth initialized
  truth<-rep(0, length(x))
  #loop over truth vector
  for (k in seq_along(truth)){
    #get the true values for outcome
    truth[k]<-ee$y[x[k]==ee$x1 & y[k]==ee$x2]
  }
  #scale it appropriately
  truth<-truth*myjson$records$data$scale[[i]][10]+5
  #chosen vector
  chosen<-rep(0, 5)
  #which one
  ch<-myjson$records$data$bonusLevel$finalChosenCell[i,]
  #mark it as 1, else 0
  chosen[x==ch$x & y ==ch$y]<-1
  #dummy data frame
  dummy<-data.frame(id=rep(i, 5), age=rep(myjson$records$data$age[i], 5), x=x, y=y, est=est, cer=cert, cond=rep(condition, 5), truth=truth, chosen=chosen)
  #bind them together
  dat<-rbind(dat, dummy)
  
}

#first 10 ids was us playing
dat<-subset(dat, id>10)
#kids have to be at least 7
dat<-subset(dat, age>=7)

#font size for plots
fontsize<-14

#error of predictions
dat$error<-abs(dat$est-dat$truth)

#create age groups
dat$agegroup<-ifelse(dat$age<9, "7-8", dat$age)
dat$agegroup<-ifelse(dat$age>=9 & dat$age <12, "9-11", dat$agegroup)
dat$agegroup<-ifelse(dat$age>18, ">18", dat$agegroup)
#condition
dat$Condition<-ifelse(dat$cond==1, "Rough", "Smooth")
#dodge
pd <- position_dodge(.8)
#color palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#age
dat$Age<-dat$agegroup
#levels of factor
dat$Age<-factor(dat$Age, levels=c("7-8", "9-11", ">18"))

#bootstrapped mean function
mmean<-function(x){
  #boot strap
  b <- boot(x, function(u,i) mean(u[i]), R = 999)
  #bias corrected estimate
  b<-boot.ci(b, type = c("bca"))
  #return the mean
  out<-as.numeric(b$t0)
  return(out)
}

#bootstrapped upper CI
upci<-function(x){
  #bootstrap
  b <- boot(x, function(u,i) mean(u[i]), R = 999)
  #bias-corrected estimate
  b<-boot.ci(b, type = c("bca"),  conf = 0.99)
  #return upper CI
  out<-as.numeric(b$bca[5])
  return(out)
}

#bootstrapped lower ci
downci<-function(x){
  #bootstrap
  b <- boot(x, function(u,i) mean(u[i]), R = 999)
  #bias-corrected estimate
  b<-boot.ci(b, type = c("bca"),  conf = 0.99)
  #return lower CI
  out<-as.numeric(b$bca[4])
  return(out)
}


#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(dat, ~Condition+Age, summarize, 
           d_ymin = max(min(error), quantile(error, 0.25) - 1.5 * IQR(error)), 
           d_ymax = min(max(error), quantile(error, 0.75) + 1.5 * IQR(error)),
           d_lower = quantile(error, 0.25),  d_middle = median(error), d_upper = quantile(error, 0.75),
           mu=mean(error))

########################################################
#Figure A: Error
########################################################

p1<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
   geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                    middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of raw data points, needs the full data frame
   geom_jitter(data=dat, aes(x = as.numeric(Age) + 0.2,  y = error,  color = Age), 
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
  xlab("Age")+ylab("Absolute error")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8","9-11", ">18"))+ggtitle("A: Error")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))


#data frame with everything needed for second plot
dp2<-ddply(dat, ~Condition+Age, summarize,  
           d_ymin = max(min(cer), quantile(cer, 0.25) - 1.5 * IQR(cer)), 
           d_ymax = min(max(cer), quantile(cer, 0.75) + 1.5 * IQR(cer)),
           d_lower = quantile(cer, 0.25),  d_middle = median(cer), d_upper = quantile(cer, 0.75),
           mu=mean(cer))


########################################################
#Figure B: Certainty
########################################################
p2<-ggplot(data = dp2) +
  #boxplot of half the size
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper,lower = d_lower,
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = Age), stat = "identity") +
  #jitter of full data
  geom_jitter(data=dat, aes(x = as.numeric(Age) + 0.2,  y = cer,  color = Age),
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  # vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  # top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +
  #facetting
  facet_wrap(~Condition)+
  #theme
  theme_minimal()+
  #mean
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #sans
  theme(text = element_text(size=fontsize,  family="sans"))+
  #colors and fills
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  xlab("Age")+ylab("Certainty")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #scale x axis with labes
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8","9-11", ">18"))+
  #title
  ggtitle("B: Certainty")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

#get the standardized estimates as well as the standardized certainties
d<-ddply(dat, ~id+Age, summarize, mu1=est[chosen==1]/sum(est), mu2=cer[chosen==1]/sum(cer))
#rejoin them as plottable data frame
d<-data.frame(id=rep(d$id, 2), Age=rep(d$Age,2), Estimate=c(d$mu1, d$mu2), j=rep(c("Estimate", "Certainty"), each=nrow(d)))
#there's one NA, instead of removing it, we penalize by adding chance value
d$Estimate<-ifelse(is.na(d$Estimate), 0.2, d$Estimate)
#one estimate is higher than .5, which we remove
d<-subset(d, Estimate<.5)

#get everything wee ned
dp3<-ddply(d, ~Age+j, summarize,  
           d_ymin = max(min(Estimate), quantile(Estimate, 0.25) - 1.5 * IQR(Estimate)), 
           d_ymax = min(max(Estimate), quantile(Estimate, 0.75) + 1.5 * IQR(Estimate)),
           d_lower = quantile(Estimate, 0.25),  d_middle = median(Estimate), d_upper = quantile(Estimate, 0.75),
           mu=mean(Estimate))
#j has to be in the right order
dp3$j<-factor(dp3$j, levels=c("Estimate", "Certainty"))

########################################################
#Figure C: Standardized estimates of chosen option
########################################################
p3<-ggplot(data = dp3) +
  #boxplot, half the size
  geom_boxplot(aes(x = as.numeric(Age)-0.2, ymin = d_lower, ymax = d_upper,
                   lower = d_lower, middle = d_middle, upper = d_upper, width = 2 * 0.2,
                   fill = Age), stat = "identity") +
  #jitter of raw data
  geom_jitter(data=d, aes(x = as.numeric(Age) + 0.2,  y = Estimate,  color = Age),
              width = 0.2 - 0.25 * 0.2, height = 0, size=0.1)+
  # vertical segment
  geom_segment(aes(x = as.numeric(Age), y = d_ymin, xend = as.numeric(Age), yend = d_ymax)) +
  # top horizontal segment
  geom_segment(aes(x = as.numeric(Age) - 0.1,  y = d_ymax, xend = as.numeric(Age),  yend = d_ymax)) +
  # top vertical segment
  geom_segment(aes(x = as.numeric(Age) - 0.1, y = d_ymin, xend = as.numeric(Age), yend = d_ymin)) +facet_wrap(~j)+
  #theme
  theme_minimal()+
  #Mean
  geom_point(aes(x = as.numeric(Age)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #Fonts
  theme(text = element_text(size=fontsize,  family="sans"))+
  #color and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #Labs
  xlab("Age")+ylab("Standardized estimate")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #Add labels
  scale_x_continuous(breaks = c(1,2,3),labels = c("7-8","9-11", ">18"))+ggtitle("C: Chosen option")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

p3
#create tikz of all plots in a grid
pdf(file = "judgements.pdf", width = 14, height = 7.5/2)
grid.arrange(p1,p2, p3, nrow=1)
dev.off()