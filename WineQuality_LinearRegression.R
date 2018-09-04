library(MLmetrics)
#library(rpart.plot)
wines <- read.csv("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Machine-Learning-with-R-datasets-master\\whitewines.csv", stringsAsFactors = TRUE)

str(wines)


hist(wines$quality)
summary(wines$quality)

sum(is.na(wines$quality))

set.seed(5)
indexes <- sample(1:nrow(wines), 0.8*nrow(wines))

train<- wines[indexes,]
dim(train)

test <- wines[-indexes,]
dim(test)

lm.model <- lm(quality ~ ., data = train)

lm.model

summary(lm.model)
cor(lm.model$fitted.values, train$quality)

lm.predict <- predict(lm.model, data = test)
summary(lm.predict)

MAPE(y_pred=lm.predict, y_true = test$quality)

dim(lm.predict)
dim(test$quality)

#cor(lm.predict, test$quality)

RMSE(lm.predict, test$quality)