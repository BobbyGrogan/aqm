# get data
setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
dfNMES <- read.csv("NMES1988.csv")

# manage data, made new column since sometimes 1,0 column is also useful
dfNMES$insurance_logical <- as.logical(dfNMES$insurance_with_number)

# partition data
set.seed(2023)
n <- nrow(dfNMES)
trainingsize <- round(n*0.6)
trainingcases <- sample(n, trainingsize)
training <- dfNMES[trainingcases, ]
test <- dfNMES[-trainingcases, ]

# make logistic model
model1 <- glm(insurance_logical ~ income, data=dfNMES, family=binomial())
summary(model1)
# B: 0.29325

# have model predict on test data
predictions <- predict(model1, test)
observations <- test$insurance_logical
errors <- observations - predictions

# create point grpah
library(ggplot)
ggplot(dfNMES, aes(x=income, y=predictions, color=insurance_with_number)) + geom_point() + geom_point(aes(y=insurance_with_number))
# E: The graph is odd because it ... it can be inferred that ... GGPLOT not working :/

# use threshold to assess model and table to represent accuracy 
predictionsTF <- predictions > 0.75
table(predictionsTF, observations)
# G: 0.5 would not be an appropriate threshold value since a model for insurance should be held to a high standard

# get sensitivity, specificity, and error rate
sensitivity <- sum(predictionsTF == TRUE & observations == TRUE)/sum(observations == TRUE)
spesificity <- sum(predictionsTF == FALSE & observations == FALSE)/sum(observations == FALSE)
# H: sensitivity is ~0.985 which means that the model usually predicts values which are actually true to be true
# H: specificity is ~0.033 which means about the model usually predicts values which are actually false to be false 

error_rate <- sum(predictionsTF != observations)/nrow(test)
# I: error rate is ~0.2253 which means that about 22.5% of the models predictions are incorrect
