setwd('C:/Users/bek2809/OneDrive - UNC-Wilmington/Senior Year/ECN 477 - 001')

library(tidyverse)
library(readxl)
library(sandwich)
library(stargazer)
library(modelsummary)
library(moments)

hmda_raw = read_excel("Data/ECN477_hmda.xlsx")

# Transform Variables from Coded Values
hmda = hmda_raw %>%
  mutate(
    deny = ifelse(s7==3,1,0),
    debt = s46,
    black = ifelse(s13==3,1,0)
  )
# Quick t-test
t.test(deny ~ black, data=hmda)

#### 4.1 -  Linear Probability Model (LPM) ####

# Simple LPM with only debt
lmp1 = lm(deny~debt, data=hmda)
summary(lmp1)

# LMP w/ debt + black
lmp2 = lm(deny ~ debt + black, data = hmda)
summary(lmp2)

# Prediction for debt=33
predict(lmp2, data.frame(debt=33, black=0))
# Prediction for debt=33 and black=1
predict(lmp2, data.frame(debt=33, black=1))

#### 4.2 - Probit Model ####

#### 4.3 Logit Model ####
logit2 = glm(deny ~ debt + black,
             data=hmda,
             family=binomial(link="logit"))
summary(logit2)

#### 4.4 Model Comparison with Binary Y ####
# pROC estimates ROC curves and AUROC
library(pROC)

# Get predicted probs for each model (lmp, probit, logit)
hmds$pr_lm = predict(lmp2, data=hmda)
hmds$pr_probit = predict(probit2, data=hmda, type="response")
hmds$pr_logit = predict(logit2, data=hmda, type="response")

# Noe we can get our ROC 
roc_lpm = roc(hmda$deny ~ hmda$pr_lpm, plot=TRUE)
roc_lpm$auc


















