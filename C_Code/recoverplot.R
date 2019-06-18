d1<-read.csv("gprecovergp.csv")
d2<-read.csv("bmtrecovergp.csv")
d3<-read.csv("rbfrecbmt.csv")
d4<-read.csv("bmtrecbmt.csv")

d1$r2<-1-d1$X.2/(-25*log(1/64))
d2$r2<-1-d2$X.2/(-25*log(1/64))
d3$r2<-1-d3$X.2/(-25*log(1/64))
d4$r2<-1-d4$X.2/(-25*log(1/64))

d1$id<-d2$id<-d3$id<-d4$id<-rep(1:160, each=8)

library(plyr)
dd1<-ddply(d1, ~id, summarize, r2=mean(r2))
dd2<-ddply(d2, ~id, summarize, r2=mean(r2))
dd3<-ddply(d3, ~id, summarize, r2=mean(r2))
dd4<-ddply(d4, ~id, summarize, r2=mean(r2))


dd<-rbind(dd1, dd2, dd3, dd4)
dd$model<-rep(c("GP", "MT", "GP", "MT"), each=nrow(dd1))
dd$producing<-rep(c("GP", "MT"), each=nrow(dd1)*2)
#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(dd, ~producing+model, summarize,
           d_ymin = max(min(r2), quantile(r2, 0.25) - 1.5 * IQR(r2)), 
           d_ymax = min(max(r2), quantile(r2, 0.75) + 1.5 * IQR(r2)),
           d_lower = quantile(r2, 0.25),  d_middle = median(r2), d_upper = quantile(r2, 0.75),
           mu=mean(r2))

dp1$model<-factor(dp1$model, levels=(c("GP", "MT")))
dd$model<-factor(dd$model, levels=(c("GP", "MT")))

library(ggplot2)
fontsize<-14
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(dd)
dd<-ddply(dd, ~id+model+producing, r2=mean(r2))
p1<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(model)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = model), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dd, aes(x = as.numeric(model) + 0.2,  y = r2,  color = model), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(model), y = d_ymin, xend = as.numeric(model), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(model)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(model) - 0.1,  y = d_ymax, xend = as.numeric(model),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(model) - 0.1, y = d_ymin, xend = as.numeric(model), yend = d_ymin)) +
  facet_wrap(~producing)+
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=fontsize,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(4,5)]))+
  scale_color_manual(values = c(cbPalette[c(4,5)]))+
  #labs
  xlab("Model")+ylab(expression("Predictive accuracy"~R^2))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2,3,4),labels = c("GP","MT", "GP", "MT"))+
  ggtitle("Recovery")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1), 
        panel.spacing.x=unit(0.2, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.title = element_text(family = "sans", margin=margin(0,0,0,0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

pdf("recoverycompare.pdf", width=8, height=4)
p1
dev.off()

sum(ddply(d3, ~id, summarize, m=median(r2))$m-ddply(d4, ~id, summarize, m=median(r2))$m<=0)
library(lsr)
cohensD(ddply(d3, ~id, summarize, m=median(r2))$m-ddply(d4, ~id, summarize, m=median(r2))$m)
mean((ddply(d3, ~id, summarize, m=median(r2))$m))
     