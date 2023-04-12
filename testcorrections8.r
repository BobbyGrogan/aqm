# Question 8

setwd("C:/Users/rgrogan1/Desktop/AQM2000")
dfDistance <- read.csv("Data/CollegeDistance.csv")

# remove all factor measurements
dfDistance$gender <- NULL
dfDistance$ethnicity <- NULL
dfDistance$fcollege <- NULL
dfDistance$home <- NULL
dfDistance$mcollege <- NULL
dfDistance$urban <- NULL
dfDistance$income <- NULL
dfDistance$region <- NULL
dfDistance$X <- NULL

# partition data
set.seed(12)
n <- nrow(dfDistance)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- dfDistance[trainingcases, ]
test <- dfDistance[-trainingcases, ]

# do the first model with all variables as potential predictors
model1 <- lm(score ~ ., data = training)
model1 <- step(model1)
#summary(model1)

# unemployment, wage, tuition, and education are the remaining variables
# score = 14.8238 (-0.1472 * unemployment) +  (0.6296 * wage) +  (2.1825 * tuition) + (2.1335 * education)

predictions1 <- predict(model1, test)
observations1 <-test$score
errors1 <- observations1 - predictions1
rmse1 <- sqrt(mean(errors1^2))
mape1 <- mean(abs(errors1 / observations1))

# do the second model with wage as a predictor
model2 <- lm(score ~ wage, data = training)
#summary(model2)

predictions2 <- predict(model2, test)
observations2 <- test$score
errors2 <- observations2 - predictions2
rmse2 <- sqrt(mean(errors2^2))
mape2 <- mean(abs(errors2 / observations2))

# get benchmarks
predictionbench <- mean(training$score)
errorsbench <- observations1 - predictionbench
rmsebench <- sqrt(mean(errorsbench^2))
mapebench <- mean(abs(errorsbench / observations1))

# The first model is more accurate since the second becuase its RMSE and MAPE are lower
