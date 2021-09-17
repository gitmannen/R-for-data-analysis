library(ggplot2)

getwd()
a<-read.csv("robustness cell number cocktail stability for R.csv")
a
str(a)

a$Tube.Name. <- factor(a$Tube.Name., c("Ref","500k cells","1.5mil cells","30mins at 4 deg","60mins at 4 deg"))

summary(a)
z<-ggplot(data=a, aes(x=Tube.Name., y=Total.CD8..Vb3....Viable, label = Total.CD8..Vb3....Viable))
z+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Tube.Name.),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Total CD8+ Vb3+ % Viable for samples with different cell number \n and different holding time of antibody cocktail prior to staining")+
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
#Summarize(Tube.Name.~Total.CD8..Vb3....Viable,
#          data=a,
#          digits = 3)

#PC10, PC8 and PC5 only
#Summarize(Ct~Group,
#          data=c,
#          digits = 3)


modela = lm(Total.CD8..Vb3....Viable~Tube.Name.,
          data = a)

summary(modela)

library(car)
Anova(modela,
      type = "II")


#install.packages('multcompView')
#install.packages('lsmeans')
library(multcompView)
library(lsmeans)
leastsquare = lsmeans(modela, pairwise ~ Tube.Name., adjust = "tukey")
leastsquare

#cld(leastsquare, alpha = 0.05, Letters = letters, adjust="tukey")


# resi = residuals(model)
# library(rcompanion)
# plotNormalHistogram(resi)
# 
# plot(fitted(model),
#      residuals(model))


library(DescTools)
DunnettTest(a$Total.CD8..Vb3....Viable,a$Tube.Name., control = "Ref", conf.level = 0.95) # this is the same at the line below
dunnofa = DunnettTest(a$Total.CD8..Vb3....Viable~a$Tube.Name., control = "Ref", conf.level = 0.95)
plot(dunnofa)

dunnofa$data.name

