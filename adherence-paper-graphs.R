
library(ggplot2)
library(tidyr)
library(dplyr)

#Loading data with 83 participants
mydata <- read.csv("C:/Users/Nicole/Documents/Work/Study 4 - Trust Survey/Analysis in R/sample83.csv")

#Recode variable name from Outcome A/B which was great to keep myself in the dark
#but is unclear for display purposes

mydata$Outcome <- dplyr::recode(mydata$Outcome, OutcomeA = "Failed", OutcomeB="Approved")
mydata2 <- gather(mydata, key, value, AccExt, AccInt, na.rm = FALSE,
       convert = FALSE, factor_key = FALSE)
mydata2$key <- dplyr::recode(mydata2$key, AccInt = "Responsibility", AccExt = "Accountability")

#colourpalette
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#0072B2", "#D55E00", "#000000")

#MHI score plot by outcome
ggplot(mydata, aes(x=Outcome, y=MHScore, fill=Outcome)) + 
  geom_boxplot(fatten=NULL)+
  stat_summary(fun.y = mean, colour="slategrey",geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 3, linetype = "solid") +
  scale_fill_manual(values=cbbPalette)+
  ylab("MH score")+
  geom_jitter(position=position_jitter(width=0.2))
  
#Accountability and responsibility split plot by outcome
ggplot(mydata2, aes(x=Outcome, y=value, fill=Outcome))+
  facet_wrap(~key)+
  geom_boxplot(fatten=NULL)+
  stat_summary(fun.y = mean, colour="slategrey",geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 3, linetype = "solid") +
  scale_fill_manual(values=cbbPalette)+
  ylab("Self-reported level of Accountability/Responsibility")+
  ylim(c(1,5))+
  geom_jitter(position=position_jitter(width=0.2))

