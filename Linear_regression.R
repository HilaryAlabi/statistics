# https://www.datacamp.com/community/tutorials/linear-regression-R
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/
# Linear regression 

library(readxl) # To read in excel files

ageandheight <- read_excel("~/ageandheight.xls")

# lm[target variable] ~ [predictor variable], data = [data source]

lmheight <- lm(height ~ age, data = ageandheight)
summary(lmheight)

# the model predicts (on average) that its height in centimeters is around 64.92 + (0.635 * 20.5) = 77.93 cm.