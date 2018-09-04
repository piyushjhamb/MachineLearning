## Caret Pre processing:


library(earth)
library(AppliedPredictiveModeling)

data(etitanic)
str(etitanic)

# convert factor to numeric using model.matrix:

head(model.matrix(survived ~ ., data = etitanic))

# ANother way of doing with caret DummyVars: This way wont work with regression like lm:
dum <- dummyVars(survived ~., data = etitanic)
# using predict to get data

head(predict(dum, newdata = etitanic))

## Zero or near Zero predictor fiding using nearzerovar functionL

data(mdrr)
# one of the variable nr11 is highly unbalance:
table(mdrrDescr$nR11)

# below method will return index of all those col which are problematic
nzv <- nearZeroVar(mdrrDescr)

# get names:

nzv_names <- nearZeroVar(mdrrDescr, names = TRUE)
nzv_names

# to use filtered data, remove all problematic col and using all rows

NewFilteredData <- mdrrDescr[, -nzv]
dim(NewFilteredData)


# FindCorrelation in Caret
#FindCorrection takes corrlation matrix
dd <- cor(NewFilteredData)

corData <- findCorrelation(dd)
# removal of such col from data

uncorData <- NewFilteredData[, corData]
dim(uncorData)


## transformation of data scale, center, boxcox

data("schedulingData")
pp <- preProcess(schedulingData[,-8], method = c("center", "scale", "YeoJohnson"))
pp

transformed_data <- predict(pp, newdata = schedulingData[, -8])
head(transformed_data)

## data partition with caret based on y variable:

data("iris")
index <- createDataPartition(iris$Species, p = 0.8, list = FALSE, times = 1)

train <- iris[index, ]
test <- iris[-index, ]

# Another usual way was sample function:
#indexes <- sample(1:nrow(titanic.2), 0.75*nrow(titanic.2))

#train<- titanic.2[indexes,]

#test <- titanic.2[-indexes,]
#dim(train)
#dim(test)

## Modelling using gbm model

library(mlbench)
library(caret)
data("Sonar")

index <- createDataPartition(Sonar$Class, p=0.75, list = FALSE)
training <- Sonar[index, ]
testing <- Sonar[-index, ]


# trainControl
set.seed(44)
fit <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

model <- train(Class ~., method = "gbm", data = training, trControl = fit, verbose = FALSE)

# tune grid

grid <- expand.grid(interaction.depth = c(1,5,9), 
                    n.trees = (1:30)*50,
                    shrinkage = .1,
                    n.minobsinnode = 20)

model2<- train(Class ~., method = "gbm", data = training, trControl = fit, verbose = FALSE, tuneGrid = grid)
model2

