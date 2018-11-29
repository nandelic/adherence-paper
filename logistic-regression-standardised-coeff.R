library(MASS)
library(aod)
library(reghelper) # has beta.glm function

#Running models for sample with 83 participants
mydata <- read.csv("C:/Users/3053836/Documents/Studies/adherence-study/sample83.csv")

#Created model with only three effects
model1 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + MHScore, family=binomial, data=mydata)
summary(model1)

#Added Attitudes
model2 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + MHScore + Attitudes, family=binomial, data=mydata)
summary(model2)

#Compared the residual deviance between model 1 and 2
anova(model1,model2,test="Chisq")
G2 = model1$deviance-model2$deviance; G2
1-pchisq(G2,df=1)

#Added IVARecall
model3 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + MHScore + Attitudes + IVARecall, family=binomial, data=mydata)
summary(model3)

#Compared residual deviance between model 2 and 3
G2 = model2$deviance-model3$deviance; G2
1-pchisq(G2,df=1)

#Compared the control variables Advisor, Survey delivery mode and days between survey and appointment
modelctrl = glm(Outcome ~ Advisor + Appointment + AccExt +AccInt + MHScore, family=binomial, data=mydata)
summary(modelctrl)
modelctrl = glm(Outcome ~ SurveyMode + Appointment + AccExt +AccInt + MHScore, family=binomial, data=mydata)
summary(modelctrl)
modelctrl = glm(Outcome ~ SurveyMode + Advisor + AccExt +AccInt + MHScore, family=binomial, data=mydata)
summary(modelctrl)

summary(model3)

#Created model with all effects
modelfull = glm(Outcome ~ AdviceMode + SurveyMode + Advisor + Appointment + Trust + AccExt +AccInt + DiscloseFin + DisclosePer + Attitudes + MHScore + IVARecall, family=binomial, data=mydata)
summary(modelfull)
G2 = model3$deviance-modelfull$deviance; G2
1-pchisq(G2,df=1)
G2 = model1$deviance-modelfull$deviance; G2
1-pchisq(G2,df=1)

#Tested adding different variables to model 3
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + Attitudes + MHScore + IVARecall + AdviceMode, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + Attitudes + MHScore + IVARecall + Trust, family=binomial, data=mydata)
summary(model4)
G2 = model4$deviance-modelfull$deviance; G2
1-pchisq(G2,df=1)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + Attitudes + MHScore + IVARecall + DiscloseFin, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + Attitudes + MHScore + IVARecall + DisclosePer, family=binomial, data=mydata)
summary(model4)

#Compared residual deviance for model 4 and full model/the previous model
G2 = model4$deviance-modelfull$deviance; G2
1-pchisq(G2,df=1)
G2 = model3$deviance-model4$deviance; G2
1-pchisq(G2,df=1)

exp(0.89614)
exp(0.84516)
exp(-0.47890)
exp(0.39412)
exp(16.32997)
exp(0.60893)
exp(1.93036)
exp(0.06502)
exp(2.34485)
exp(-1.85306)
exp(-0.03796)
exp(-0.41868)
exp(-0.37974)
exp(0.73107)
exp(-0.44063)
exp(-0.38617)
exp(-0.04451)
exp(-1.94695)
exp(2.54717)
exp(-0.41138)
exp(-0.04183)
exp(-1.50294)
exp(2.14910)
exp(-0.03157)
exp(-1.38087)
exp(1.77689)

#Retrieving confidence intervals for full model
confint.default(modelfull)

#Retrieving confidence intervals for model 4
confint.default(model4)
#Retrieving confidence intervals (from standardised coeff) for model 4
beta(model4)$coefficients
CI_lower <- coefficients(model4)[2] - 1.96*beta(model4)$coefficients[,2]
CI_upper <- coefficients(model4)[2] + 1.96*beta(model4)$coefficients[,2]

test <- exp(beta(model4)$coefficients[2,2])/(1+exp(beta(model4)$coefficients[2,2]))

#Retrieving standardised coefficients for all models
beta(model4)

#Retrieving standardised odds ratios for the three significant main predictors
exp(-1.55) #0.21 responsibility
exp(1.48) #4.39 accountability
exp(0.85) #2.34 MHI
exp(0.79) #2.20 attitudes
exp(0.73) #2.08 memory recall
exp(-0.51) #0.60 personal disclosure


