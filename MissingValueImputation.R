# Kaggle Data Challenge

# Part 1, Missing Values
# dataset - Building data San Francisco Building permit

# Follow Approach from: https://datascienceplus.com/missing-value-treatment/
# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
#https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/


options(scipen=999)

library(caret)
library(ggplot2)
library(dplyr)
library(readr)

# read Data

building <- read_csv("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Building_Permits.csv\\Building_Permits.csv")

str(building)

dim(building)
summary(building)

summary(building$Zipcode)
summary(building$`Street Suffix`)

class(building$Zipcode)

## Handling missing values with MICE
library(mice)

#md.pattern() - It returns a tabular form of missing value present in each variable in a data set.
md.pattern(building)


# Four ways to handle missing values:

# delete rows
# delete variable (colomn)

# Impute with Mean/Median/mode
 # like estimated cost is continuous variable:
summary(building$`Estimated Cost`)

# ANother Package
library(Hmisc)
mean(building$rev)
building$imputedRevisedCost <- with(building, impute(building$`Revised Cost`, mean))



# another way to impute using aregImpute - https://www.rdocumentation.org/packages/Hmisc/versions/4.1-0/topics/aregImpute




# Impute with Nearest neighbour computation algo like KNN.

# handle using MICE
# Two step process - mice() and then impute using complete()

library(mice)
imputedBuilding <- mice(building, m = 5, maxit = 5, method = "pmm", seed = 44)  # perform mice imputation, based on random forests.

## Above code will result into error and the solutions is to use CART method to https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586

smalldata <- building[1:20000, ]

imputedBuilding <- mice(smalldata, m = 5, maxit = 5, method = "cart", seed = 47) 

summary(imputedBuilding)
CompletedrevisedCost2 <- complete(imputedBuilding)  # generate the completed data.
sum(anyNA(CompletedrevisedCost2))





