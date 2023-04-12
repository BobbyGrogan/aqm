library(e1071)

setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
source("C:Users/rgrogan1/Desktop/AQM2000/Data/BabsonAnalyticsC.R")
df <- read.csv("NBdataset.csv")

everycolumn <- colnames(df)
df[everycolumn] <- lapply(df[everycolumn], as.factor)

n <- nrow(df)
trainingsize <- round(n*0.6)
trainingcases <- sample(n, trainingsize)
training <- df[trainingcases, ]
test <- df[-trainingcases, ]

model <- naiveBayes(positive_tweet ~ ., data = training)

predictions <- predict(model, test)
observations <- test$positive_tweet

error_bench <- benchmarkErrorRate(training$positive_tweet, test$positive_tweet)
error <- sum(predictions != observations)/nrow(test)

table(predictions, observations)

#new code
model$table$cute

# this table says given positive, 61% chance cute is in the comment

limited <- df[df$cute == "1", ] 
odds <- sum(limited$positive_tweet == "1")/sum(limited$positive_tweet = "0")

limited <- df[df$puppy == "1", ] 
odds <- sum(limited$positive_tweet == "1")/sum(limited$positive_tweet = "0")

