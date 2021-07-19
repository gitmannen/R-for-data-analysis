library(ggplot2)

getwd()
a<-read.csv("RST Robustness cell number.csv")
a
str(a)

a$Group <- factor(a$Group, c("PC","NTC","Ref","300k","500k","800k"))

summary(a)
z<-ggplot(data=a, aes(x=Group, y=Ct, label = Ct))
z+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Group),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Ct values for samples with different cell number in the ROX channel")+
  xlab("Samples")+
  ylab("Ct value in the ROX channel")+
  theme(axis.title = 
          element_text(size = 18),
        legend.title = 
          element_text(size = 18),
        legend.text = 
          element_text(size = 16),
        strip.text = 
          element_text(size = 16),
        axis.text = 
          element_text(size = 16),
        axis.ticks.x.bottom = 
          element_blank(),
        plot.title =
          element_text(size = 20,
                       hjust = 0.5)
        )+
  
  #add in labels to the plot
  geom_text(check_overlap = T, #avoid overlapping the labels
            size = 3,
            show.legend = F, #remove the 'a' in the legend
            #hjust = 0,
            #nudge_x = -0.6,
            #vjust = 0,
            #nudge_y = -0.01,
            aes(colour = Group) #changes the colour to match the Operator colour
            )

b<-a[a$Group!="PC" & a$Group!="NTC",]


library(FSA)
Summarize(Ct~Group,
          data=b,
          digits = 3)


model = lm(Ct~Group,
          data = b)
summary(model)

library(car)
Anova(model,
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
