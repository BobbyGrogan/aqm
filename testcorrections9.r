# Question 9

# load in data and libraries 
#setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
library(ggplot2)
dfDistance <- read.csv("Data/CollegeDistance.csv")

# remove unecessary columns
dfDistance$X <- NULL
dfDistance$gender <- NULL
dfDistance$ethnicity <- NULL
dfDistance$fcollege <- NULL
dfDistance$home <- NULL
dfDistance$mcollege <- NULL
dfDistance$urban <- NULL
dfDistance$region <- NULL

# add boolean income column
dfDistance$income = as.integer(ifelse(dfDistance$income == 'low', 0, 1))
dfDistance$incomeTF <- as.logical(dfDistance$income)

# partition data
set.seed(12)
n <- nrow(dfDistance)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- dfDistance[trainingcases, ]
test <- dfDistance[-trainingcases, ]

# create logistic model
model1 <- glm(income ~ score, data = training, family = binomial())
#summary(model1)

increase <- exp(0.04243)
print(increase)

# For every unit the odds increase by 1.043343

# run model to make predictions and then show predictions on graph
predictions <- predict(model1, test, type = "response")
ggplot(test, aes(x = score, y = predictions, color = income)) + geom_point() + geom_point(aes(y = income))

# The graph is not an s-curve which means that score is not a very good predictor for income

predictionsTF <- predictions > 0.25
observations <- test$incomeTF
table(predictionsTF, observations)

# The curve does not go above 0.5 so there would not be true predictions in the table

errorrate <- sum(predictionsTF != observations) / nrow(test)
sensitivity <- sum(predictionsTF == TRUE & observations == TRUE) / sum(observations == TRUE)
specificity <- sum(predictionsTF == FALSE & observations == FALSE) / sum(observations == FALSE)

print(errorrate)
print(sensitivity)
print(specificity)

# 47.2% of predictions were not accurate
# 74.5% of actual trues were predicted true
# 43.6% of actual falses were predicted false
