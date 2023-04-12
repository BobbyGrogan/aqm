setwd("C:/Users/rgrogan1/Desktop/AQM2000/data")

library(rpart)
library(rpart.plot)

df <- read.csv("eBayAuctions.csv")
df2 <- read.csv("crTreeLesson.csv")

df$Category <- as.factor(df$Category)
df$Currency <- as.factor(df$Currency)
df$EndDay <- NULL
df$Competitive <- as.factor(df$Competitive)
df$ClosePrice <- NULL

# df2$success >- as.factor(df2$success)
source("c:/Users/rgrogan1/Desktop/AQM2000/data/BabsonAnalyticsC.R")

n <- nrow(df)
trainingsize <- round(n* .6)
trainingcases <- sample(n, trainingsize)
training <- df[trainingcases, ]
test <- df[-trainingcases, ]

stopping_rules <- rpart.control(minsplit = 2, minbucket = 1, cp = 0)

model <- rpart(Competitive ~ ., data = training)
rpart.plot(model)
rpart.plot(model, tweak = 1.1, varlen = 4, faclen = 4)
predictions <- predict(model, test)

View(predictions)

model2 <- rpart(success ~ ., data = df2, control = stopping_rules)
rpart.plot(model2)

predictions <- predict(model, test, type = "class")
observations <- test$Competitive

error_rate <- sum(predictions !=  observations)/nrow(test)
error_bench <- benchmarkErrorRate(training$Competitve, test$Competitive)

model <- rpart(Competitive ~ ., data = training, control = stopping_rules)
rpart.plot(model)

predictions_overfit <- predict(model, test, type = "class")
error_rate_overfit <- sum(predictions_overfit != observations)/nrow(test)

model <- easyPrune(model)
predictions_pruned <- predict(model, test, type = "class")
error_rate_pruned <- sum(predictions_pruned != observations)/nrow(test)

