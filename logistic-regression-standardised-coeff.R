library(MASS)
library(aod)
library(car)
library (lme4)
library(modEvA)
library(reghelper) # has beta.glm function
options(scipen=999)

#Running models for sample with 83 participants
mydata <- read.csv("C:/Users/3053836/Documents/Studies/adherence-study/sample.csv")

#Convert attitudes to mean instead of sum
mydata$Attitudes <- mydata$Attitudes/3


##### Step 1: Full model
modelfull = glm(Outcome ~ AdviceMode + SurveyMode + Advisor + Appointment + Trust + AccExt +AccInt + DiscloseFin + DisclosePer + Attitudes + MHScore + IVARecall, family=binomial, data=mydata)
summary(modelfull)

##### Step 2: Created model with only the four significant effects + controls
model1 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + MHScore + IVARecall, family=binomial, data=mydata)
summary(model1)

# Compared the new simplified model 1 with full model
G2 = model1$deviance-modelfull$deviance; G2
1-pchisq(G2,df=1) # significant difference

### Step 3: Added each remaining predictor one at a time, Attitudes resulted in the biggest variance change
model2 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + MHScore + IVARecall + Attitudes, family=binomial, data=mydata)
summary(model2)

# Compared model 1 with model 2
G2 = model1$deviance-model2$deviance; G2
1-pchisq(G2,df=1)

# Compared model 2 with full model
G2 = model2$deviance-modelfull$deviance; G2
1-pchisq(G2,df=1) # No significant difference, this is best fitting reduced model

### Step 4: Generate confidence intervals
confint.default(model2)

### Step 5: Generate standardised coeff.
beta(model2)






