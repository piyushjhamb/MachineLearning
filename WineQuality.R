library(rpart)
library(rpart.plot)
wines <- read.csv("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Machine-Learning-with-R-datasets-master\\whitewines.csv", stringsAsFactors = TRUE)

str(wines)

hist(wines$quality)
summary(wines$quality)

sum(is.na(wines$quality))

set.seed(2)
indexes <- sample(1:nrow(wines), 0.8*nrow(wines))

train<- wines[indexes,]
dim(train)

test <- wines[-indexes,]
dim(test)


m.rpart <- rpart(quality ~ ., data = train)
m.rpart
summary(m.rpart)
rpart.plot(m.rpart, digits = 3)

p.rpart <- predict(m.rpart, test)

summary(p.rpart)
summary(test$quality)





