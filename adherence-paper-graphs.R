
library(ggplot2)
library(tidyr)
library(dplyr)

#Loading data with 83 participants

mydata <- read.csv("C:/Users/3053836/Documents/Studies/adherence-study/analysis/sample.csv")

#Recode variable name from Outcome A/B which was great to keep myself in the dark
#but is unclear for display purposes

mydata$Outcome <- dplyr::recode(mydata$Outcome, OutcomeA = "Non-adherence", OutcomeB="Adherence")

#colourpalette
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#0072B2", "#D55E00", "#000000")

#MHI score plot by outcome
ggplot(mydata, aes(x=Outcome, y=MHScore, fill=Outcome)) + 
  geom_boxplot(fatten=NULL)+
  stat_summary(fun.y = mean, colour="slategrey",geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1.5, linetype = "solid") +
  scale_fill_manual(values=cbbPalette)+
  ylab("MH score")+
  geom_jitter(position=position_jitter(width=0.2, height=0.2)) +
  theme(legend.position = "none") 

# Accountability plot
ggplot(mydata, aes(x=Outcome, y=AccExt, fill=Outcome))+
  geom_boxplot(fatten=NULL)+
  stat_summary(fun.y = mean, colour="slategrey",geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1.5, linetype = "solid") +
  scale_fill_manual(values=cbbPalette)+
  ylab("Self-reported level of Accountability")+
  geom_jitter(position=position_jitter(width=0.2, height=0.1)) + 
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(1, 5.2))

# Responsibility plot
ggplot(mydata, aes(x=Outcome, y=AccInt, fill=Outcome))+
  geom_boxplot(fatten=NULL)+
  stat_summary(fun.y = mean, colour="slategrey",geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1.5, linetype = "solid") +
  scale_fill_manual(values=cbbPalette)+
  ylab("Self-reported level of Responsibility")+
  ylim(c(1,5.2))+
  geom_jitter(position=position_jitter(width=0.2, height=0.1)) + 
  theme(legend.position = "none") 
