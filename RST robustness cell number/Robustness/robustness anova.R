library(ggplot2)

getwd()
# setwd("..\\Users\\Lion\\Documents\\Lion Local\\RST and RMT qualification\\rst")
a<-read.csv("RST Robustness cell number.csv")
a
str(a)
#a$Operator == 'O1'
# a[a$Operator == 'O1',2] <- "Operator 1"
# a[a$Operator == 'O2',2] <- "Operator 2"
# 
# summary(a[a$Operator == "Operator 1",][a$Sample == "PC",])
# OperatorPC <- a[a$Operator == "Operator 1",][a$Sample == "PC",]
# mean(OperatorPC)
# OperatorPC[,4]
# apply(OperatorPC,2,mean)
#Box Plot of Intermediate Precision (Operator to Operator) in the ROX channel
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
        axis.title.x = 
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


library(FSA)
Summarize(Ct~Group,
          data=a,
          digits = 3)

# dayToDayVar <- ggplot(data = a, aes(x= Operator, y = ROX))
# dayToDayVar+
#   geom_boxplot()+
#   geom_jitter(aes(colour=Day))


#Additional ANOVA test using the R handbook/ Mix of Two-way and One-way ANOVA.
# if(!require(psych)){install.packages("psych")}
# if(!require(FSA)){install.packages("FSA")}
# if(!require(Rmisc)){install.packages("Rmisc")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(car)){install.packages("car")}
# if(!require(multcompView)){install.packages("multcompView")}
# if(!require(lsmeans)){install.packages("lsmeans")}
# if(!require(rcompanion)){install.packages("rcompanion")}
# 
# typeof(a)
# 
# library(psych)
# headTail(a)
# a
# 
# library(FSA)
# Summarize(ROX~Operator + Sample,
#           data=a,
#           digits = 3)        
# 
# boxplot(ROX~Operator + Sample,
#         data = a)
# 
# library(rcompanion)
# Sum = groupwiseMean(data = a,
#                     var = "ROX",
#                     group = c("Operator","Sample"),
#                               conf = 0.95,
#                               digits = 3,
#                               tradition = F,
#                               percentile = T)
# Sum
# 
# library(ggplot2)
# ggplot(Sum,aes(x = Sample, y = Mean))+
#   geom_errorbar(aes(ymin = Percentile.lower,
#                     ymax = Percentile.upper),
#                 width = 0.05,
#                 size = 0.5)+
#   geom_point(shape = 15,
#               size = 4,
#              aes(colour = Operator))+
#   theme_bw()+
#   theme(axis.title = element_text(face = "bold"))+
#   ylab("Mean Rox, ct")
# 
# 
model = lm(Ct~Group,
          data = a)
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
