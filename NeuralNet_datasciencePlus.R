library(MASS)

data <- Boston
str(data)
apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)


# To use Normalize data, create a fnction
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}


# Apply Function
data.norm <- as.data.frame(lapply(data, normalize))

summary(data.norm$medv)
library(neuralnet)


# split data for test/train
index <- sample(1:nrow(data.norm),round(0.75*nrow(data.norm)))
train.norm <- data[index,]
test.norm <- data[-index,]


# using two hidden layer and 13/5 neurons:

set.seed(123)
# Specify Formula:
n <- names(train.norm)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train.norm,hidden=c(5,3),linear.output=T, rep = 10)

plot(nn)

# prediction done by compute:
pr.nn <- compute(nn,test.norm[,1:13])


# revert back min/max normalization:
pr.nn_ <- pr.nn$net.result*(max(data.norm$medv)-min(data.norm$medv))+min(data.norm$medv)
test.r <- (test.norm$medv)*(max(data.norm$medv)-min(data.norm$medv))+min(data.norm$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test.norm)

# plot Neuralnet 

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')






