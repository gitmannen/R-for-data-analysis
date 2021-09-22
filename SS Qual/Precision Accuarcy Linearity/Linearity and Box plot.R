library(ggplot2)

getwd()
# setwd("..\\Users\\Lion\\Documents\\Lion Local\\RST and RMT qualification\\rst")
a<-read.csv("CD8posVbetapos percent viable after normalization with mock for R.csv")
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


# setting the factors

summary(a)
a$Sample
a$Sample = factor(a$Sample, levels=c('EP0 (Mock)','EP20','EP40','EP60','EP80','EP100'))






dataset<-a[c(1,6)]
dataset
str(dataset)
dataset$Sample
avgByEPConc <- aggregate(x=dataset$EP,by=list(dataset$Sample),FUN =mean)
# aggregate(x=dataset$CD8.Vb3....Viable,by=list(dataset$Sample),FUN =sd)

avgByEPConc$Group.1 <- c(0,20,40,60,80,100)
avgByEPConc$Group.1 = factor(avgByEPConc$Group.1, levels=c('0','20','40','60','80','100'))
avgByEPConc$EP <- c(0,20,40,60,80,100)
str(avgByEPConc)

colnames(avgByEPConc)=c("EP_Percent","CD8posVb3pos_Percent_Viable","EP")



# plotting a regression line
linplot <- ggplot(data=avgByEPConc, aes(x=EP, y=CD8posVb3pos_Percent_Viable, label = CD8posVb3pos_Percent_Viable))
linplot+
  geom_point(aes(x=EP, y=CD8posVb3pos_Percent_Viable))+
  geom_smooth(aes(x=EP, y=CD8posVb3pos_Percent_Viable), method =lm, formula = y~x)
  #stat_summary(aes(x=EP, y=CD8posVb3pos_Percent_Viable))





dataset2<- a[c(2,6)]
dataset2$EPConcFactor <- factor(dataset2$EP.Conc, levels=c(0,20,40,60,80,100))

linplot <- ggplot(data=dataset2, aes(x=EP.Conc, y=EP))
linplot+
  geom_point(aes(x=EP.Conc, y=EP))+
  geom_smooth(aes(x=EP.Conc, y=EP), method =lm, formula = y~x)+
  scale_x_continuous(name="EP Percent",n.breaks=11,labels=waiver())+
  scale_y_continuous(name="CD8+Vb3+ % Viable",n.breaks=20,labels=waiver())+
  boxplot(aes(x=dataset2$EPConcFactor, y=EP))
  
boxplo

warnings()










#Box Plot of Intermediate Precision (Operator to Operator) for Total (CD8+/-) Vb3+ % Viable


z<-ggplot(data=a, aes(x=Day, y=CD8.Vb3....Viable, label = CD8.Vb3....Viable))
z+
  facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
  scale_y_continuous(n.breaks = 10)+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Operator),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Intermediate Precision (Operator to Operator) for Total CD8+ Vb3+ % Viable")+
  ylab("Total CD8+ Vb3+ % Viable")+
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
            aes(colour = Operator) #changes the colour to match the Operator colour
            )

  

#Box Plot of Intermediate Precision (Day to Day) for Total (CD8+/-) Vb3+ % Viable
x<-ggplot(data=a, aes(x=Operator, y=CD8.Vb3....Viable, label = CD8.Vb3....Viable))
x+
  facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
  scale_y_continuous(n.breaks = 10)+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size = 5,
              aes(colour = Day),
              alpha = 0.5
  )+
  ggtitle("Box Plot of Intermediate Precision (Day to Day) for Total CD8+ Vb3+ % Viable")+
  ylab("for Total CD8+ Vb3+ % Viable")+
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
            # hjust = 0,
            # nudge_x = -0.1,
            #vjust = 0,
            #nudge_y = -0.01,
            aes(colour = Day) #changes the colour to match the Operator colour
  )




#Responsive

#these 2 libraries are needed for the tidyverse evaluation so that you can you can input arguments to the aes()
#see https://stackoverflow.com/questions/58786357/how-to-pass-aes-parameters-of-ggplot-to-function and https://ggplot2.tidyverse.org/dev/articles/ggplot2-in-packages.html
install.packages("gapminder")
library(gapminder)  
library(rlang)


newfunction <- function(xval,grouper){
  v1<-ggplot(data=a, aes(x=.data[[xval]], y=CD8.Vb3....Viable, label =CD8.Vb3....Viable)) #here you can see how the aes() arguments refer to the arguments in the function
  
  #when the channel is ROX, we need to show all three samples, so you need the facet_wrap because there are 3 samples: PC, NTC and Sample

  v1+
    facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
    scale_y_continuous(n.breaks = 10)+
    geom_boxplot(outlier.alpha = 0)+
    geom_jitter(size = 5,
                aes(colour = .data[[grouper]]),
                alpha = 0.5
    )+
    ggtitle(paste("Box Plot of Intermediate Precision (", grouper, " to ", grouper, ") for Total CD8+ Vb3+ % Viable ",sep =""))+
    ylab(paste("Total CD8+ Vb3+ % Viable"))+
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
              # hjust = 0,
              # nudge_x = -0.1,
              #vjust = 0,
              #nudge_y = -0.01,
              aes(colour = .data[[grouper]]) #changes the colour to match the Operator colour
    )

  

    
}

library(FSA)
aSummary <- Summarize(CD8.Vb3....Viable ~ Sample+Operator,
                     data=a,
                     digits = 3)

bSummary <- Summarize(CD8.Vb3....Viable ~ Sample+Day,
                      data=a,
                      digits = 3)
# DAdata<-aSummary[1:6,c(1,2,4)]
# DAdata$EPPercent <- c("0","100","20","40","60","80")
# DAdata$EPPercent <- factor(DAdata$EPPercent, levels=c('0','100','20','40','60','80'))
# 
# regressor = lm(formula = mean ~ EPPercent,
#                data = DAdata)
# 
# ggplot(data = DAdata, aes(x=EPPercent, y=mean))+
#   geom_point(aes(x=EPPercent, y=mean))+
#   geom_line()
# 
# 
# plot(DAdata$EPPercent,DAdata$mean)

aSummary
bSummary


modela = lm(CD8.Vb3....Viable~Operator+Day+Sample,
            data = a)

summary(modela)

library(car)
Anova(modela,
      type = "II")

library(multcompView)
library(lsmeans)
leastsquare = lsmeans(modela, pairwise ~ Operator + Day + Sample, adjust = "tukey")
leastsquare




newfunction("Operator","Day")
newfunction("Day","Operator") #arguments are as follows (xval, channel, grouper)
# xval is what you want the size of the box plot to depend on. e.g. if you are looking at day to day variability,
# the xval should be the operator, so you will see 1 box plot for operator 1 and another boxplot for operator 2 the factor you are comparing between (e.g. day to day or operator to operator)
# channel is the channel that you are interested in (e.g. ROX or FAM)
# grouper is what you want to compare. e.g. day to day or operator to operator. if you put day, then you should see that you the points in your box plot is colour cooded according to the day of operation