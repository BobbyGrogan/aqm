# QUIZ 1
# using the dataset Budget Food, get the dataset
dfFood = read.csv("Data/BudgetFood.csv")

# gets the library used for graphing
library(ggplot2)

# creates a histogram of the total expenditures of these housholds
# bin number should be the closest integer to the square root of the size of the dataset
ggplot(dfFood, aes(x=totexp)) + geom_histogram(bins=120)

# creates a bar graph of the column 'town' to show which towns the sample households come from
ggplot(dfFood, aes(x=town)) + geom_bar()

# creates a box plot which represents the distribution of food expenditure
ggplot(dfFood, aes(x=wfood)) + geom_boxplot()

print(dfFood)

# box plot interpretation: the median percentage of total expenditure which the household has spent on food is around 0.31
# 50% of the households spend between ~0.26 and ~0.48
# there are some outliers who spend more than ~80% of their expenditure on food
