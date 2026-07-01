setwd('C:/Users/bek2809/OneDrive - UNC-Wilmington/Senior Year/ECN 477 - 001')

#Load packages
library(readxl)
library(tidyverse)


# Import data
class_data<- read_excel("Data/caschool.xlsx")

# How would we make a scatter plot for the data?
plot1 <- ggplot(class_data) + 
  geom_point(aes(x=str, y=testscr)) +
  ggtitle("Scatterplot of Test Score and Class Size") +
  xlab("Student-Teacher Ratio") +
  ylab("Average Test Score") +
  xlim(10,30) +
  theme_classic()
plot(plot1)

# Confidence Intervals for Regression Coeff.
  # confint(reg)

# Correlation Coefficient
cor.test(class_data$str, class_data$testscr)

#### 2.1 Simple Linear Regression (SLR) ####
reg1 = lm(testscr ~ str, data=class_data)
summary(reg1)
  # Interpret B0 and B1 into real words
# Adding intervals to the plot
plot2 <- plot1 +
  geom_smooth(aes(x=str, y=testscr), method=lm, color = 'darkgreen')
plot(plot2)

class_data <- cbind(class_data, predict(reg1, interval="prediction"))

#add prediction to the plot
plot3 <- plot2 +
  geom_line(data=class_data, aes(x=str, y=lwr), color ='#006666', linetype="dotted") +
  geom_line(data=class_data, aes(x=str, y=upr), color ='#006666', linetype="dotted")
plot(plot3)  



#### 2.3 -  ####

#### 2.4 - Model Comparison ####


#running new regressions
reg3 = lm(testscr ~ str + el_pct +meal_pct , class_data)
reg4 = lm(testscr ~ str + el_pct +calw_pct , class_data)
reg5 = lm(testscr ~ str + el_pct +meal_pct +calw_pct , class_data)

library(modelsummary)        # gives us organized regression tables
library(pandoc)              # allows us to write to docx

# getting a table for one model (reg3)
modelsummary(reg3,
             stars = TRUE)

# getting a table for multiple models
modelsummary(list(reg1, reg3, reg4, reg5),
             stars= TRUE,
             fmt=2,
             title = "Regression Summary for California School Data",
             output = "caschool_regs.docx")

      
             



#### 2.5 - Issues with OLS ####

# Perfect Multicolinearity
class_data$low_el <- if(class_data$el_pct < 20, 1, 0)
reg_dummy = lm(testscr ~ high_el + low_el, data=class_data)
summary(reg_dummy)
# R drops low_el due to perfect multicolinearity

# imperfect multicolinearity
  #install 'car' package for 'vif' function
library(car)
vif(reg5)

# Heteroskedascisity
  # add to modelsummary() the option of "vcove = "HC1"
  # will need to install sandwich
library(sandwich)





