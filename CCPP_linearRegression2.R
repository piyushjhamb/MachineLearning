library(readxl)

setwd("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\CCPP")

powerData <- read_excel("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\CCPP\\Folds5x2_pp.xlsx", sheet="Sheet1")

# split data in train and Test

index <- sample(1:nrow(powerData), 0.8*nrow(powerData))

train <- powerData[index, ]
dim(train)

test <- powerData[-index, ]
dim(test)

predictionModel <- lm(PE~., data = train)
summary(predictionModel)

par(mfrow=c(2,2))
plot(predictionModel)
