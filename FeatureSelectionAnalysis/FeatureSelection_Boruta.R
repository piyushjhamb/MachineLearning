# Boruta feature selection : 
#https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/


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


## Now use boruta to find out the most useful variables:

boruta.train<- Boruta(Loan_Status~., data = trainSet)

print(boruta.train)

plot(boruta.train)

## To take decision on tentative attributes:
final.boruta <- TentativeRoughFix(boruta.train)
final.boruta


## Get list of confirmed attributes:

final_att <- getSelectedAttributes(final.boruta, withTentative = F)
final_att


### NOw use Caret's RFE algorithm for feature selection:

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

rfe.train <- rfe(trainSet[,1:631], trainSet[,632], rfeControl=control)

rfe.train

plot(rfe.train)







