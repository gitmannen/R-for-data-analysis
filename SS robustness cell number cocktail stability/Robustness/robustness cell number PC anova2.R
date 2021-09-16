library(ggplot2)

getwd()
a<-read.csv("Exp_20210830_ ER2 SS Robustness Draq7 and staining time for R.csv")
a
str(a)

a$Tube.Name. <- factor(a$Tube.Name., c("Ref","Stain 10min","Stain 25min","Draq7 15min","Draq7 30min"))

summary(a)
z<-ggplot(data=a, aes(x=Tube.Name., y=Total.CD8..Vb3....Viable, label = Total.CD8..Vb3....Viable))
z+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Tube.Name.),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Total CD8+ Vb3+ % Viable for samples with different antibody staining time \n and different DRAQ7 staining time")+
  xlab("Samples")+
  ylab("Total CD8+ Vb3+ % Viable")+
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
Summarize(Tube.Name.~Total.CD8..Vb3....Viable,
          data=a,
          digits = 3)

#PC10, PC8 and PC5 only
Summarize(Ct~Group,
          data=c,
          digits = 3)


modela = lm(Total.CD8..Vb3....Viable~Tube.Name.,
          data = a)

summary(modela)

library(car)
Anova(modela,
      type = "II")


install.packages('multcompView')
install.packages('lsmeans')
library(multcompView)
library(lsmeans)
leastsquare = lsmeans(modela, pairwise ~ Tube.Name., adjust = "tukey")
leastsquare

cld(leastsquare, alpha = 0.05, Letters = letters, adjust="tukey")


# resi = residuals(model)
# library(rcompanion)
# plotNormalHistogram(resi)
# 
# plot(fitted(model),
#      residuals(model))


# testing aov() and TukeyHSD()
anovaona =aov(formula = Total.CD8..Vb3....Viable~Tube.Name.,data = a) # this will shoow you only the sum of squares and the def of freedom
summary(anovaona) # use this to get your p-value
tuk = TukeyHSD(anovaona,"Tube.Name.",conf.level = 0.95) # the p-values for the pairwise comparison is title under 'p adj'
plot(tuk,las=3, cex.axis = 0.5) # use this visualize the confidence intervals
summary(tuk)
tuk
typeof(tuk)

# testing code from statskingdom

value<-c(66.96,67.38,67.70,69.92,71.24,71.81,70.98,71.70,71.92,70.75,70.99,71.40)
group1 <- c(rep("Stain10", 3), rep("stain25", 3), rep("D7 15", 3), rep("D7 30", 3))
my.dataframe<-data.frame(value, group1)
res.aov <- aov(value ~ group1, data = my.dataframe)
summary(res.aov)
TukeyHSD(res.aov)
# results appear to tbe the same as using aov() and TukeyHSD()

# dunnett's test for comparison with a reference group
library(DescTools)
DunnettTest(a$Total.CD8..Vb3....Viable,a$Tube.Name., control = "Ref", conf.level = 0.95) # this is the same at the line below
dunnofa = DunnettTest(a$Total.CD8..Vb3....Viable~a$Tube.Name., control = "Ref", conf.level = 0.95)
plot(dunnofa)
