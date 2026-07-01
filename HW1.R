# 477 Homework 1

# Install tidyverse and readexcel
library(tidyverse)
library(readxl)
library(moments)


#Set working directory
setwd("~/Library/CloudStorage/OneDrive-UNC-Wilmington/Senior Year/ECN 477 - 001")
# Q1 - 5: MC

# Q6
Data = ECN477_UR_Parties %>% filter(!is.na(UR))
View(Data)
# 1st Moment - Mean
mean_ur = mean(Data$UR)
  # 5.77

# 2nd Moment - Variance
var_ur = var(Data$UR)
  # 2.84

# 3rd Moment - Skewness
skewness_ur = skewness(Data$UR)
  # 0.92

# 4th Moment - Kurtosis
kurtosis_ur = kurtosis(Data$UR)
  # 4.16

# Histogram
ggplot(data= Data) +                        
  geom_histogram(aes(x=UR), color="navy", fill="lightblue") +             # creates histogram for URate
  geom_text(aes(x=UR), label="Mean AHE", x=100, y = 1000, angle = 90, color="black") + 
  ggtitle("UR 1953-2025") + # add title
  xlab("UR") +        # Add x label
  ylab("Count") +     # Add y label
  theme_classic()     # Theme    

# Q7 - Avg UR under R and D presidents
  # Dem. President (1)
urate = as.numeric(Data$UR)
party = as.numeric(Data$Dem)
mean(urate[party==1])

  # Rep. President (0)
mean(urate[party==0])

# Q8 t-test
t.test(Data$UR)

# Q9 - Null and alt. hypothesis


# Q10 - Open answer