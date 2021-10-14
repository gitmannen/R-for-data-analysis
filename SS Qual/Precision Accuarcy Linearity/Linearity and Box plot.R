library(ggplot2)

getwd()
# setwd("..\\Users\\Lion\\Documents\\Lion Local\\RST and RMT qualification\\rst")
a<-read.csv("CD8posVbetapos percent viable after normalization with mock for R.csv")
a
str(a)

# setting the factors
summary(a)
a$Sample
a$Sample = factor(a$Sample, levels=c('EP0 (Mock)','EP20','EP40','EP60','EP80','EP100'))


dataset<-a[c(1,6)]
dataset
str(dataset)
dataset$Sample
avgByEPConc <- aggregate(x=dataset$EP,by=list(dataset$Sample),FUN =mean)
daggsd<-aggregate(x=dataset$EP,by=list(dataset$Sample),FUN =sd) # use the aggregate function to pass a function across the data
daggmean<-aggregate(x=dataset$EP,by=list(dataset$Sample),FUN =mean)


avgByEPConc$Group.1 <- c(0,20,40,60,80,100)
avgByEPConc$Group.1 = factor(avgByEPConc$Group.1, levels=c('0','20','40','60','80','100'))
avgByEPConc$EP <- c(0,20,40,60,80,100)
str(avgByEPConc)

colnames(avgByEPConc)=c("EP_Percent","CD8posVb3pos_Percent_Viable","EP")



# plotting a regression line, not required as this plots a regression line of the means, not taking the individual values
linplot <- ggplot(data=avgByEPConc, aes(x=EP, y=CD8posVb3pos_Percent_Viable, label = CD8posVb3pos_Percent_Viable))
linplot+
  geom_point(aes(x=EP, y=CD8posVb3pos_Percent_Viable))+
  geom_smooth(aes(x=EP, y=CD8posVb3pos_Percent_Viable), method =lm, formula = y~x)
  #stat_summary(aes(x=EP, y=CD8posVb3pos_Percent_Viable))

# getting the regression model. optional but the data can be counter-checked with the result from stat_poly_eq()
regressor = lm(formula = CD8posVb3pos_Percent_Viable ~ EP,
                   data = avgByEPConc)
regressor

# installing ggpubr
#install.packages("ggpubr")
library(ggpubr)

#install.packages("ggpmisc")
library(ggpmisc)

dataset2<- a[c(2,6)]
dataset2$EPConcFactor <- factor(dataset2$EP.Conc, levels=c(0,20,40,60,80,100))
str(dataset2)

bplot <- ggplot(data=dataset2, aes(x=EP.Conc, y=EP))
# box plot but using the ggpmisc to give the equation of the regression line
dev.cur()
bplot+
  geom_smooth(aes(x=EP.Conc,y=EP),method=lm, se = TRUE, formula = y~x,)+
  #geom_boxplot(aes(colour=EPConcFactor), outlier.alpha = 0)+ # optional, but the graph will be too confusing
  stat_summary(aes(color=EPConcFactor),fun = mean,geom="crossbar", width = 5)+ # if you don't want to use box plot you can just use this to draw the mean line at each group
  geom_point(aes(color=EPConcFactor),size=1, alpha=1)+
  scale_x_continuous(name="EP Percent",n.breaks=11,labels=waiver())+
  scale_y_continuous(name="CD8+Vb3+ % Viable",n.breaks=20,labels=waiver())+
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, coef.digits = 5, f.digits = 5, p.digits = 10, rr.digits = 4, size = 6)+ # just use this function from ggpmisc to get the equation and r2 value
  guides(color = guide_legend(title = "EP Percent"))+
  ggtitle("Linear Regression of CD8+Vb3+ % viable against Percent EP cells in various EP:Mock mixing ratios")+
  theme(axis.title = 
          element_text(size = 18),
        legend.title = 
          element_text(size = 12),
        legend.text = 
          element_text(size = 12),
        axis.text = 
          element_text(size = 12),
        plot.title =
          element_text(size = 16,
                       hjust = 0.5))#+
  #coord_cartesian(ylim=c(65,80)) # use this to zoom to the upper limit
  

# bplot
# bplot+
#   #geom_point(aes(x=EP.Conc, y=EP))+
#   #geom_smooth(aes(x=EP.Conc, y=EP), method =lm, formula = y~x)+
#   geom_boxplot(data=dataset2, aes(group=EPConcFactor), outlier.alpha = 0)+
#   geom_jitter(aes(color=EPConcFactor))+
#   scale_x_continuous(name="EP Percent",n.breaks=11,labels=waiver())+
#   scale_y_continuous(name="CD8+Vb3+ % Viable",n.breaks=20,labels=waiver())+
#   geom_smooth(aes(x=EP.Conc,y=EP),method=lm, se = FALSE, formula = y~x)+
#   stat_cor(label.y = 80,digits = 3) + # stat_cor uses the ggpubr package
#   stat_regline_equation(label.y = 77) # stat_regline_equation uses the ggpubr package, but the number of sig digits cannot be cahnged
#   #geom_line(aes(x= avgByEPConc$EP_Percent ,y=predict(regressor)))