# Association Rules:

library(caret)
library(ggplot2)
library(dplyr)
library(arules)

# read Data

hrdata <- read.transactions("C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Machine-Learning-with-R-datasets-master\\groceries.csv", sep=",")

str(hrdata)
dim(hrdata)
head(hrdata, 10)


# finding support information:

inspect(hrdata[1:5])

# frequency plot of first 5 coloumn, bydefault alphabet order

itemFrequency(hrdata[,1:5])

# item frequency plot: support plot: where item must appear more then 10% time, i.e support 0.1 

itemFrequencyPlot(hrdata, support = 0.1)

# top 20 items sale
itemFrequencyPlot(hrdata, topN = 20)

# image function to view sparse matrix data: 5 rows with all 169 columns

image(hrdata[1:5])


# Apriori command to find out the actionable insights:

gr <- apriori(hrdata, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))


summary(gr)

#inspect rules:

inspect(gr[1:5])


#sorted rules based on lift:
inspect(sort(gr, by = "lift")[1:5])

# rules of specific item : like berrries:

berryRules <- subset(gr, items %in% "berries")

# or rules containing berry and yogurt

berryRules2 <- subset(gr, items %in%c("berries", "yogurt"))

# Also you can use & or | ! with combination to find out the subsets of rules and later sort them for finding best rules

# write rules to csv

write(berryRules, sep = ",", file = "C:\\Users\\piyush.jhamb\\Documents\\CPBAE\\RegressionPractice\\Machine-Learning-with-R-datasets-master\\berryRules.csv", row.names = FALSE, quote = TRUE) 

