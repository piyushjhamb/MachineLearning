# Data
library(dplyr)
library(data.table)
data <- read.csv(file.choose(), header=T)
data$is_attributed <- as.factor(data$is_attributed)

# Problem
prop.table(table(data$is_attributed))

# Data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Addressing Class Imbalance
library(ROSE)
under <- ovun.sample(is_attributed ~ ., 
                     data = train, 
                     method = "under", 
                     N = 376)$data

# Prediction Model
library(randomForest)
under <- under[,c(1:5, 8)]
model <- randomForest(is_attributed ~ ., data = under)

# Evaluation
library(e1071)
library(caret)
confusionMatrix(predict(model, test), 
                test$is_attributed, 
                positive='1')


## Use of XGBoost on this data, Also try to read more train data for better results
 # Read full train file:
train <- fread("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\Kaggle\\AdTalkingData\\train.csv\\mnt\\ssd\\kaggle-talkingdata2\\competition_files\\train.csv", skip=124903890, nrows=60000000, 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "attributed_time", "is_attributed"),
               showProgress = FALSE)

# Read Test:
test  <- fread("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\Kaggle\\AdTalkingData\\test.csv\\test.csv", showProgress = FALSE) 


# Create sparse matrix:


# Create matrix - One-Hot Encoding for Factor variables

trainm <- sparse.model.matrix( ~ ., data = train[,-"is_attributed"])
head(trainm)
train_label <- train[,"is_attributed"]
train_matrix <- xgb.DMatrix(data = as.matrix(train), label = train_label)

test_matrix <- xgb.DMatrix(data = as.matrix(test))



xgb_params <- list("eval_metric" = "rmse")
watchlist <- list(train = train_matrix, test = test_matrix)

# eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       eta = 0.1,
                       max.depth = 3,
                       scale_pos_weight=99.76,
                       gamma = 0,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 33)






# File Submission on Kaggle
new <- read.csv(file.choose(), header=T)
new1 <- new[,-1]
new1$attributed_time <- 0
p <- predict(model, new1)


d <- data.frame(click_id = new$click_id, is_attributed = p)
write.csv(d, "C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\Kaggle\\AdTalkingData\\Piyush_KAGGLE.csv", row.names = F)
