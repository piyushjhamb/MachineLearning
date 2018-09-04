# load libary

library(psych)
#setwd("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\CCPP")

insurance <- read.csv("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Machine-Learning-with-R-datasets-master\\insurance.csv", stringsAsFactors = TRUE)

str(insurance)

hist(insurance$charges)


# find out relationship between each variable:

cor(insurance[c("age", "bmi", "children", "charges")])
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

model <- lm(charges ~ ., data = insurance)

model

summary(model)

par(mfrow=c(2,2))
plot(model)


# Apply random Forest to this model
library(ranger)
model_RF<- ranger(charges ~ ., data = insurance, num.trees = 500, respect.unordered.factors = "order")
model_RF
summary(model_RF)
library(MLmetrics)
charges_predicted_RF <- predict(model_RF, newdata = insurance)
RMSE(charges_predicted_RF, insurance$charges)


# RMSE
library(MLmetrics)
charges_predicted <- predict(model, data = insurance)
RMSE(charges_predicted, insurance$charges)

MAPE(charges_predicted, insurance$charges)


# Adaptive boosting for this model
library(adabag)
set.seed(66)
ada <- boosting(charges~., data = insurance)




# kwayCrossValidation Plan
library(vtreat)
splitPlan <- kWayCrossValidation(1338, 10, NULL, NULL)
k<- 10
str(splitPlan)
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(charges ~ ., data = insurance[split$., ])
  mpg$pred.cv[split$charges] <- predict(model, newdata = insurance[split$charges, ])
}



