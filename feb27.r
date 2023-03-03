# setwd("C:/Users/...")
library(ggplot2)
#install.packages("caret")
library(caret)

df = read.csv("Data/BostonHousing.csv")

# manage data
df$CHAS <- NULL
df$RAD <- NULL
df$ISHIGHVAL <- NULL

# puts all values into z scores
#process <- preProcess(df[,0:10],c("center", "scale"))
process <- preProcess(df[,0:10],c("range"))
df <- predict(process, df)

set.seed(1234)
n = nrow(df)
size = (n * 0.6)
trainingcases = sample(n, size)
training = df[trainingcases, ]
test = df[-trainingcases, ]

source("Data/BabsonAnalytics.R")

model <- knnreg(MEDV ~ ., data=training, k=3)
#summary(model)

# predict 
predictions <- predict(model, test)

# KPI
observations <- test$MEDV
errors <- observations - predictions
mape <- mean(abs(errors/observations))
print(mape)
rmse <- sqrt(mean(errors^2))
print(rmse)

errors_bench <- observations - mean(training$MEDV)
mape_bench <- mean(abs(errors_bench/observations))
rmse_bench <- sqrt(mean(errors_bench^2))
print(mape_bench)
print(rmse_bench)
