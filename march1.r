setwd("C:/Users/ekeitt1/Desktop/AQM2000/data")
df <- read.csv("BostonHousing.csv")

df$CHAS <- NULL
df$RAD <- NULL
#leave as factor for knn model
df$ISHIGHVAL <- as.factor(df$ISHIGHVAL)
#remove bc too similar to ISHIGHVAL
df$MEDV <- NULL

#file for knn classification
source("c:/Users/ekeitt1/Desktop/AQM2000/data/BabsonAnalyticsC.R")

#Processor <- preProcess(df[,1:10],c ("center", "scale"))
Processor <- preProcess(df[,1:10],c ("range"))
df <- predict(Processor, df)

set.seed(1234)
n <- nrow(df)
trainingsize <- round(n*.6)
trainingcases <- sample(n, trainingsize)
training <- df[trainingcases,]
test <- df[-trainingcases,]

#method to get appropriate k value for classification not regression
k_best <- kNNCrossVal(ISHIGHVAL ~ ., training)
model <- knn3(ISHIGHVAL ~ ., data = training, k = k_best)

predictions_normalize <- kNN(ISHIGHVAL ~ ., training, test, k = k_best)
observations <- test$ISHIGHVAL
error_rate_normalize <- sum(predictions_normalize != observations)/nrow(test)


error_bench <- benchmarkErrorRate(training$ISHIGHVAL, test$ISHIGHVAL)
predictions_no_standardization <- kNN(ISHIGHVAL ~ ., training, test, k = 3)
error_rate_no_standardization <- sum(predictions_no_standardization != observations)/nrow(test)

