data(iris)
str(iris)

library(ggplot2)

qplot(Petal.Length, Petal.Width, data=iris, color = Species)

library(e1071)

mysvm <- svm(Species ~ ., data = iris)

mysvm2 <- svm(Species ~ ., data = iris, cost = 0.1, scale = FALSE)

pred <- predict(mysvm2, iris)

tab2 <- table(pred, iris$Species)
tab2
mysvm2$index
summary(mysvm2)

# tune method of e1071 to do the crossvalidation on the dataset
tunesvm <- tune(svm, Species ~., data = iris, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tunesvm)

# tune method keeps the best model, 

bestmodel <- tunesvm$best.model

summary(bestmodel)

predBest <- predict(bestmodel, iris)

tab2Best <- table(predBest, iris$Species)
tab2Best


mysvm$index

summary(mysvm)
plot(mysvm, iris$Species)