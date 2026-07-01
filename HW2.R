# Install packages
library(readxl)
library(tidyverse)
library(moments)
library(sandwich)
library(lmtest)
library(modelsummary)

# Section 1 - Education and Wages

#Q1 - interpret the coefficients
reg1 = lm(wage ~ educ, data=ECN477_cps)
summary(reg1)

#Q2 - explain controlling for heteroskedasticity
coeftest(reg1, vcov=vcovHC(reg1,type="HC1"))

#Q3 - MLR
reg2 = lm(wage~ educ + black + female, data=ECN477_cps)
summary(reg2)
2.03815 - 1.999438

#Q4
modelsummary(reg2,
             stars=c("*"=.10,"**"=.05,"***"=0.1))

#Q5
reg3 = lm(wage ~ educ+black+female+northeast+midwest+south+west, data=ECN477_cps)
q5_worker = data.frame(educ=12,
                       black=1,
                       female=0,
                       northeast=0,
                       midwest=0,
                       south=1,
                       west=0)
predict(reg3,q5_worker, data=ECN477_cps)


# Section 2 - Capital Asset Pricing Model

#Q6

dis_rp= ECN477_capm$dis - ECN477_capm$riskfree
ge_rp= ECN477_capm$ge - ECN477_capm$riskfree
gm_rp= ECN477_capm$gm - ECN477_capm$riskfree
ibm_rp= ECN477_capm$ibm - ECN477_capm$riskfree
msft_rp= ECN477_capm$msft - ECN477_capm$riskfree
xom_rp= ECN477_capm$xom - ECN477_capm$riskfree

mean(dis_rp)
mean(ge_rp)
mean(gm_rp)
mean(ibm_rp)
mean(msft_rp)
mean(xom_rp)

var(dis_rp)
var(ge_rp)
var(gm_rp)
var(ibm_rp)
var(msft_rp)
var(xom_rp)

# Q7
mkt_rp = ECN477_capm$mkt - ECN477_capm$riskfree

reg_dis = lm(dis_rp ~ mkt_rp,)
summary(reg_dis)

reg_ge = lm(ge_rp ~ mkt_rp)
summary(reg_ge)

reg_gm = lm(gm_rp ~ mkt_rp)
summary(reg_gm)

reg_ibm = lm(ibm_rp ~ mkt_rp)
summary(reg_ibm)

reg_msft = lm(msft_rp ~ mkt_rp)
summary(reg_msft)

reg_xom = lm(xom_rp ~ mkt_rp)
summary(reg_xom)

# Q8 - Q10: Open ended