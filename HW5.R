setwd("C:/Users/Ben/OneDrive - UNC-Wilmington/Senior Year/ECN 477 - 001/477 Project/Project Data")

# Imported nels data

nels_raw = nels
View(nels_raw)

# Q1
nels_raw$COLLEGE <- ifelse(nels_raw$PSECHOICE %in% c(1, 2), 1, 0)
mean(nels_raw$COLLEGE) * 100

# Q2
colMeans(nels_raw[, c("GRADES", "FAMINC", "FAMSIZ", "FEMALE", "BLACK")])

# Q3
mean(nels_raw$PSECHOICE[nels_raw$COLLEGE == 1] == 1) * 100

mean(nels_raw$BLACK[nels_raw$PSECHOICE == 1]) * 100

# Q4
q4_lpm <- lm(COLLEGE ~ GRADES + FAMINC + FAMSIZ + PARCOLL + FEMALE + BLACK, data = nels_raw)
summary(q4_lpm)

# Q5
predict(q4_lpm, newdata = data.frame(GRADES = 5, FAMINC = mean(nels_raw$FAMINC), 
  FAMSIZ = 5, PARCOLL = 1, FEMALE = 1, BLACK = 1))

# Q6
new_data <- data.frame(
  GRADES  = 5,
  FAMINC  = mean(nels_raw$FAMINC),
  FAMSIZ  = 5,
  PARCOLL = 1,
  FEMALE  = c(0, 1, 0, 1),
  BLACK   = c(0, 0, 1, 1)
)
predict(q4_lpm, newdata = new_data)

# Q7
q7_lpm <- lm(COLLEGE ~ GRADES + FAMINC + FAMSIZ, data = nels_raw)
summary(q7_lpm)

# Q8
# Predict at mean GRADES vs top 5th percentile
base <- data.frame(GRADES = mean(nels_raw$GRADES), FAMINC = mean(nels_raw$FAMINC),
                   FAMSIZ = mean(nels_raw$FAMSIZ), PARCOLL = mean(nels_raw$PARCOLL),
                   FEMALE = mean(nels_raw$FEMALE), BLACK = mean(nels_raw$BLACK))

top5 <- base
top5$GRADES <- 2.635

predict(q7_lpm, newdata = base)
predict(q7_lpm, newdata = top5)
predict(q7_lpm, newdata = top5) - predict(q7_lpm, newdata = base)

# Q9
logit <- glm(COLLEGE ~ GRADES + FAMINC + FAMSIZ + PARCOLL + FEMALE + BLACK,
             data = nels_raw, family = binomial(link = "logit"))
summary(logit)

# Q10









