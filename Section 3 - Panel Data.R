setwd('C:/Users/bek2809/OneDrive - UNC-Wilmington/Senior Year/ECN 477 - 001')


library(tidyverse)
library(readxl)
library(sandwich)
library(stargazer)
library(modelsummary)
library(moments)

#### Section 3 - Panel Regressions ####

death<- read_excel("Data/ECN477_fatality.xlsx")
View(death)

# Create new variable 'fatal' = traffic deaths per 10k people
death = death%>%
  mutate(fatal = 10000*mrall)

# Lets only looks at one year of data (cross sectional): 1982
reg1982 = lm(fatal ~ beertax, data = death %>%filter(year==1982))
summary(reg1982)
  # This b1 is not statistically sig, so it doesnt really mean much to us
# lets look at 1988
reg1988 = lm(fatal ~ beertax, data = death %>%filter(year==1988))
summary(reg1988)
  # This b1 is almost 3x higher


#### 3.2 Panel Regression With 2 Time Periods ####

#Option 1: Using lm()
# Compute the change in fatality rate (FR) and beer tax (BT)

death_diff = left_join(
    filter(death, year==1982) %>% select(state, fatal, beertax),
    filter(death, year==1988) %>% select(state, fatal, beertax),
    by="state",
    suffix = c("82","88")
  ) %>%
  mutate(
    diff_fatal = fatal88 - fatal82,
    diff_beertax = beertax88 - beertax82
  )

# Diff in Diff model
reg_did = lm(diff_fatal ~ diff_beertax, data=death_diff)
summary(reg_did)


# Option 2: Use feols() - USE THIS FOR PANEL DATA
library(fixest)
reg_did2 = feols(
  d(fatal) ~ d(beertax),             #computes the difference in a variable
  data=death %>% filter(year %in% c(1982, 1988)),
  panel.id = ~ state + year
)
summary(reg_did2)


# Entity level Fixed Effect (FE) only
reg_fe1 = feols(fatal ~ beertax | state, data=death)
summary(reg_fe1)

# Time level fixed Effect (FE) only
reg_fe2 = feols(fatal ~ beertax | year, data=death)
summary(reg_fe2)

# 2 Way Fixed Effect Model
reg_twfe = feols(fatal ~ beertax| state + year, data=death)

#### 3.4 Clustered Standard Errors ####
reg_twfe_cluster = feols(fatal ~ beertax| state+year, data = death, vcov=~state)

#### Summary ####
# Pooled Panel regression (All years, no fixed effects)
reg_pool = feols(fatal ~ beertax, data=death)

# Kitchen Sink Regression 
reg_twfe_all = feols(fatal ~ beertax + mlda + jaild + comserd + vmiles + unrate | state + year,
                     data=death,
                     vcov=state)
modelsummary(list("Pooled"=reg_pool, "state FE"=reg_fe1, "Year FE"=reg_fe2, "Two-Way"=reg_twfe, "Comprehensive"=reg_twfe_all)
             XXXX)



