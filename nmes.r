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

# TEST PART 1


# get data
setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
dfDiamonds <- read.csv("diamonds.csv")

# manage data
dfDiamonds$carat <- NULL
dfDiamonds$clarity <- NULL
dfDiamonds$color <- NULL
dfDiamonds$X <- NULL

# partition data
set.seed(2023)
n <- nrow(dfDiamonds)
trainingsize <- round(n*0.6)
trainingcases <- sample(n, trainingsize)
training <- dfDiamonds[trainingcases, ]
test <- dfDiamonds[-trainingcases, ]

# create best subset linear model
model1 <- lm(price ~ ., data=dfDiamonds, family=binomial())
model1 <- step(model1)
summary(model1)
# D: price = 20655.994 + 10686.632(carat) - 200.467(depth) - 109.950(table) -1293.267(x) + 69.234(y)

# create standard linear model
model2 <- lm(price ~ y, data=dfDiamonds)
summary(model2)

# run models through test data
predictions1 <- predict(model1, test)
predictions2 <- predict(model2, test)

# get residuals of both models
observations <- test$price
errors1 <- observations - predictions1
errors2 <- observations - predictions2

# find mape and rmse for each model
mape1 <- mean(abs(errors1/observations))
rmse1 <- sqrt(mean(errors1^2))
mape2 <- mean(abs(errors2/observations))
rmse2 <- sqrt(mean(errors2^2))

# get benchmarks
prediction_benchmark <- mean(training$price)
errors_benchmark <- observations - prediction_benchmark
rmse_benchmark <- sqrt(mean(errors_benchmark^2))
mape_benchmark <- mean(abs(errors_benchmark/observations))

# I: RMSE for model1 is 1827.52 which means the average residual for model 1 is $1827.52
# I: RMSE for model2 is 1854.13 which means the average residual for model 2 is $1854.13
# I: MAPE for model1 is 0.65 which means the average percent difference of the predicted versus actual value for model 1 is 0.65 
# I: MAPE for model1 is 0.59 which means the average percent difference of the predicted versus actual value for model 1 is 0.59
# I: RMSE benchmark is 3992.51 which means without a model the average residual would be $3992.51
# I: MAPE benchmark is 1.87 which means without a model the percentage difference of the predicted versus actual value would be 1.87

