# Caret problem from Analytics vidya: 
#https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/


options(scipen=999)

library(caret)
library(ggplot2)
library(lubridate)
library(readr)
library(neuralnet)
library(Boruta)

# read Data

loan <- read.csv("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\FeatureSelectionAnalysis\\train.csv", stringsAsFactors = TRUE)

str(loan)

## Here we have about 12 indepedent variable and one dependent variable, so lets find out various ways to do feature selection:
## it is useful when you have all indepedent variable as numeric and your outcome is also numeric

# one way to find out is to do correlation matrix with cutoff:

corMatrix<- cor(loan[,1:12])

print(corMatrix)
# Find highly correlated matrix:
highcor <- findCorrelation(corMatrix, cutoff = 0.75)
print(highcor)

plot(highcor)


## Another methos is to use train function of caret, and then find out varImp on data:

# Check NAs

sum(is.na(loan))

# impute missing values using caret
# preprocess is just class and use in applying methods, whereas predict.preprocess will actually give you the results
preProcValues <- preProcess(loan, method = c("knnImpute","center","scale"))


library(RANN)
loan.processed <- predict(preProcValues, loan)

sum(is.na(loan.processed))

str(loan.processed)

# Convert loan status to numeric

loan.processed$Loan_Status <- ifelse(loan.processed$Loan_Status == "N", 0, 1)
class(loan.processed$Loan_Status)

# one hot endcoding - converting every factor level to mumerical number

dmy <- dummyVars(" ~.", data = loan.processed, fullRank = T)

# now predict will give us the new dummy variables:
loan.transformed <- data.frame(predict(dmy, loan.processed))

# converting dependent variable back to categorical
loan.transformed$Loan_Status <- as.factor(loan.transformed$Loan_Status)



# create test / train data using partition:
index <- createDataPartition(loan.transformed$Loan_Status, p = .75, list = FALSE)

trainSet <- loan.transformed[ index,]
testSet <- loan.transformed[-index,]

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
#mtry <- sqrt(ncol(x))
#tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Loan_Status~., data=trainSet, method="rf", trControl=control)

