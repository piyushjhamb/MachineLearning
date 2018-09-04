options(scipen=999)

library(caret)
library(ggplot2)
library(lubridate)
library(readr)
library(neuralnet)

# read Data

concrete <- read.csv("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Machine-Learning-with-R-datasets-master\\concrete.csv", stringsAsFactors = FALSE)

str(concrete)

# Because neuralnet works on normalize data, make sure the data is normalize:
# create normalize function for data:

# Also can use mmnorm function in R

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}



concrete_norm <- as.data.frame(lapply(concrete, normalize))

summary(concrete$strength)

# normalized strength data
summary(concrete_norm$strength)

training <- concrete_norm[1:773, ]
testing <- concrete_norm[774:1030, ]

set.seed(123)

# Make sure y~. is not working for neuralnet package somehow so need to add column name for every col
m<- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = training)

plot(m)

# predict using neuralnet it uses a compute command

predicted_strength <- compute(m, testing[1:8])

predicted_values <- predicted_strength$net.result

# compute the accuracy of model using correlation between predicted values and actual values
 
cor(predicted_values, testing$strength)

# To Improve model strength we can add hidden neorons in the model build command which will add to the hidden layer of network

set.seed(73)
updated_model <- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = training, hidden = 3)

plot(updated_model)

updatedStrength <- compute(updated_model, testing[1:8])
predicted_values_updated <- updatedStrength$net.result



# Check accuracy now
cor(predicted_values_updated, testing$strength)



## Without normalize data 
set.seed(88)
train_1 <- concrete[1:773, ]
test_1 <- concrete[774:1030, ]

model_1 <- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = train_1, hidden = 3)
plot(model_1)

summary(model_1)

# predicting values

pred_model1 <- compute(model_1, test_1[1:8])

pred_values_1_neorons <- pred_model1$neurons
predicted_values_1 <- pred_model1$net.result

cor(predicted_values_1, test_1$strength)
# shows only about 20% of the correlation, so very bad performance.





