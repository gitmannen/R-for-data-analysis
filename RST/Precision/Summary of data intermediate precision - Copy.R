library(ggplot2)

getwd()
# setwd("..\\Users\\Lion\\Documents\\Lion Local\\RST and RMT qualification\\rst")
a<-read.csv("precision op to op repeat.csv")
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
z<-ggplot(data=a, aes(x=Day, y=ROX, label = ROX))
z+
  facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
  scale_y_continuous(n.breaks = 10)+
  geom_boxplot(outlier.alpha = 0,
               alpha = 0.5)+
  geom_jitter(size = 5,
              aes(colour = Operator),
              alpha = 0.3
              )+
  ggtitle("Box Plot of Intermediate Precision (Operator to Operator) in the ROX channel")+
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
            aes(colour = Operator) #changes the colour to match the Operator colour
            )

  
#Box Plot of Intermediate Precision (Operator to Operator) in the FAM channel
y<-ggplot(data=a, aes(x=Day, y=FAM, label = FAM))
y+
  #facet_wrap(.~.,nrow = 1, scales = "free_y", labeller = label_both)+
  scale_y_continuous(n.breaks = 10)+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size = 5,
              aes(colour = Operator),
              alpha = 0.5
  )+
  ggtitle("Box Plot of Intermediate Precision (Operator to Operator) in the FAM channel")+
  ylab("Ct value in the FAM channel")+
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
            aes(colour = Operator) #changes the colour to match the Operator colour
  )


#Box Plot of Intermediate Precision (Day to Day) in the ROX channel
x<-ggplot(data=a, aes(x=Operator, y=ROX, label = ROX))
x+
  facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
  scale_y_continuous(n.breaks = 10)+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size = 5,
              aes(colour = Day),
              alpha = 0.5
  )+
  ggtitle("Box Plot of Intermediate Precision (Day to Day) in the ROX channel")+
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
            # hjust = 0,
            # nudge_x = -0.1,
            #vjust = 0,
            #nudge_y = -0.01,
            aes(colour = Day) #changes the colour to match the Operator colour
  )


#Box Plot of Intermediate Precision (Day to Day) in the FAM channel
w<-ggplot(data=a, aes(x=Operator, y=FAM, label = FAM))
w+
  #facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
  scale_y_continuous(n.breaks = 10)+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size = 5,
              aes(colour = Day),
              alpha = 0.5
  )+
  ggtitle("Box Plot of Intermediate Precision (Day to Day) in the FAM channel")+
  ylab("Ct value in the FAM channel")+
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


newfunction <- function(xval,channel,grouper){
  v1<-ggplot(data=a, aes(x=.data[[xval]], y=.data[[channel]], label = .data[[channel]])) #here you can see how the aes() arguments refer to the arguments in the function
  
  #when the channel is ROX, we need to show all three samples, so you need the facet_wrap because there are 3 samples: PC, NTC and Sample
  if (channel == "ROX") {
    v1+
      facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
      scale_y_continuous(n.breaks = 10)+
      geom_boxplot(outlier.alpha = 0)+
      geom_jitter(size = 5,
                  aes(colour = .data[[grouper]]),
                  alpha = 0.5
      )+
      ggtitle(paste("Box Plot of Intermediate Precision (", grouper, " to ", grouper, ") in the ", channel, " channel", sep =""))+
      ylab(paste("Ct value in the", channel, "channel"))+
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
  
  #if channel is not ROX (i.e. ROX), then you don't need the face_wrap because there is only 1 sample
  else{
    v1+
      #facet_wrap(.~Sample,nrow = 1, scales = "free_y", labeller = label_both)+
      scale_y_continuous(n.breaks = 10)+
      geom_boxplot(outlier.alpha = 0)+
      geom_jitter(size = 5,
                  aes(colour = .data[[grouper]]),
                  alpha = 0.5
      )+
      ggtitle(paste("Box Plot of Intermediate Precision (", grouper, " to ", grouper, ") in the ", channel, " channel", sep =""))+
      ylab(paste("Ct value in the", channel, "channel"))+
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
    
}
    
  


newfunction("Operator","FAM","Day") #arguments are as follows (xval, channel, grouper)
# xval is what you want the size of the box plot to depend on. e.g. if you are looking at day to day variability,
# the xval should be the operator, so you will see 1 box plot for operator 1 and another boxplot for operator 2 the factor you are comparing between (e.g. day to day or operator to operator)
# channel is the channel that you are interested in (e.g. ROX or FAM)
# grouper is what you want to compare. e.g. day to day or operator to operator. if you put day, then you should see that you the points in your box plot is colour cooded according to the day of operation


a

DayVarRox <- a[a$Operator == 'Op1',c(1,2,3,5)]
aov(ROX~Sample)


install.packages('FSA')
install.packages('car')

library(FSA)
Summarize(ROX~Operator+Sample,
          data=DayVarRox,
          digits = 3)


model = lm(ROX~Sample,
           data = DayVarRox)
model
summary(model)

library(car)
Anova(model,
      type = "II")


install.packages('multcompView')
install.packages('lsmeans')
library(multcompView)
library(lsmeans)
leastsquare = lsmeans(model, pairwise ~ Operator, adjust = "tukey")
leastsquare

cld(leastsquare, alpha = 0.05, Letters = letters, adjust="tukey")


# resi = residuals(model)
# library(rcompanion)
# plotNormalHistogram(resi)
# 
# plot(fitted(model),
#      residuals(model))