library(ggplot2)

getwd()
a<-read.csv("Exp_20210830_ ER2 SS Robustness Draq7 and staining time draq7 only viable percent of singlets for R.csv")
a
str(a)

a$Tube.Name. <- factor(a$Tube.Name., c("Ref","Draq7 15min","Draq7 30min"))

summary(a)
z<-ggplot(data=a, aes(x=Tube.Name., y=Viable...Singlets, label = Viable...Singlets))
z+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Tube.Name.),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Viable % Singlets for samples with dfferent DRAQ7 staining time")+
  xlab("Samples")+
  ylab("Viable % Singlets")+
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
            aes(colour = Tube.Name.) #changes the colour to match the Operator colour
            )


library(FSA)
#Ref, 300k, 500k, and 800k only
#Summarize(Tube.Name.~Viable...Singlets,
#          data=a,
#          digits = 3)

#PC10, PC8 and PC5 only
#Summarize(Ct~Group,
#          data=c,
#          digits = 3)


modela = lm(Viable...Singlets~Tube.Name.,
          data = a)

summary(modela)

library(car)
Anova(modela,
      type = "II")


# install.packages('multcompView')
# install.packages('lsmeans')
library(multcompView)
library(lsmeans)
leastsquare = lsmeans(modela, pairwise ~ Tube.Name., adjust = "tukey")
leastsquare

# cld(leastsquare, alpha = 0.05, Letters = letters, adjust="tukey")


# resi = residuals(model)
# library(rcompanion)
# plotNormalHistogram(resi)
# 
# plot(fitted(model),
#      residuals(model))





# testing purposes using aov() and TukeyHSD to check if the results are the same as the anova() and the lsmeans() methods


#anovaona =aov(formula = Viable...Singlets~Tube.Name.,data = a) # this will shoow you only the sum of squares and the def of freedom
#summary(anovaona) # use this to get your p-value which is title under "p adj"
#tuk = TukeyHSD(anovaona,"Tube.Name.",conf.level = 0.95)
#summary(tuk) # use this visualize the confidence intervals



# dunnett's test for comparison with a reference group
library(DescTools)
DunnettTest(a$Viable...Singlets,a$Tube.Name., control = "Ref", conf.level = 0.95) # this is the same at the line below
dunnofa = DunnettTest(a$Viable...Singlets~a$Tube.Name., control = "Ref", conf.level = 0.95)
plot(dunnofa)
