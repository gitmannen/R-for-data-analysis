library(ggplot2)

getwd()
a<-read.csv("RMT robustness cell number PC.csv")
a
str(a)

a$Group <- factor(a$Group, c("PC10","PC8","PC5","NTC","Ref","300k","500k","800k"))

summary(a)
z<-ggplot(data=a, aes(x=Group, y=Ct, label = Ct))
z+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Group),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Ct values for samples with different cell number \n and different volumes of PC in the ROX channel")+
  xlab("Samples")+
  ylab("Ct value in the ROX channel")+
  theme(axis.title = 
          element_text(size = 16),
        legend.title = 
          element_text(size = 16),
        legend.text = 
          element_text(size = 16),
        strip.text = 
          element_text(size = 14),
        axis.text = 
          element_text(size = 14),
        axis.ticks.x.bottom = 
          element_blank(),
        plot.title =
          element_text(size = 18,
                       hjust = 0.5)
        )+
  
  #add in labels to the plot
  geom_text(check_overlap = T, #avoid overlapping the labels
            size = 4.3,
            show.legend = F, #remove the 'a' in the legend
            hjust = 0.5,
            #nudge_x = -0.6,
            vjust = 1.5,
            #nudge_y = -0.01,
            aes(colour = Group) #changes the colour to match the Operator colour
            )+
  stat_summary(fun = mean, geom = 'errorbar', aes(ymax = ..y..,ymin=..y..),
               width = 0.75, linetype = 'dashed')

#Ref, 300k, 500k, and 800k only
b<-a[a$Group!="PC10" & a$Group!="PC8" & a$Group!="PC5" & a$Group!="NTC",]

#PC10, PC8 and PC5 only
c<-a[a$Group != "Ref" & a$Group != "300k" & a$Group != "500k" & a$Group != "800k",]

library(FSA)
#Ref, 300k, 500k, and 800k only
Summarize(Ct~Group,
          data=b,
          digits = 3)

#PC10, PC8 and PC5 only
Summarize(Ct~Group,
          data=c,
          digits = 3)


modelb = lm(Ct~Group,
          data = b)

modelc = lm(Ct~Group,
           data = c)

summary(model)

library(car)
Anova(modelb,
      type = "II")

Anova(modelc,
      type = "II")

install.packages('multcompView')
install.packages('lsmeans')
library(multcompView)
library(lsmeans)
leastsquare = lsmeans(model, pairwise ~ Group, adjust = "tukey")
leastsquare

cld(leastsquare, alpha = 0.05, Letters = letters, adjust="tukey")


# resi = residuals(model)
# library(rcompanion)
# plotNormalHistogram(resi)
# 
# plot(fitted(model),
#      residuals(model))
