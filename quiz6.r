# Get directory, dataframe, and libraries
setwd("C:/Users/rgrogan1/Desktop/AQM2000/")
df <- read.csv("Data/cutedog.csv")
library(e1071)

# I chose to keep all columns and change them to factor
all <- colnames(df)
df[all] <- lapply(df[all], as.factor)

# Partition the data
set.seed(13)
n <- nrow(df)
trainingsize <- round(n* .6)
trainingcases <- sample(n, trainingsize)
training <- df[trainingcases, ]
test <- df[-trainingcases, ]

# Create the model and run it
model <- naiveBayes(PositiveSentiment ~ ., data = training) 
predictions <- predict(model, test)
observations <- test$PositiveSentiment
table(predictions, observations)

# Get the error rate
error_rate <- sum(predictions !=  observations)/nrow(test)

# 0.265 of positive statements contain "cute"
allpos <- df$PositiveSentiment == "1"
cute <- df$cute == "1"
hascute <- sum(cute) / sum(allpos)
print(hascute) 

# 0.147 of positive statements contain "not"
not <- df$not == "1"
hasnot <- sum(not) / sum(allpos)
print(hasnot)

# For  negative statement with "cute" there would be 8 positive statements with "cute"
limited <- df[df$cute == "1", ]
odds <- sum(limited$PositiveSentiment == "1") / sum(limited$PositiveSentiment == "0")
print(odds)

# For  negative statement with "like" there would be 1.5 positive statements with "like"
limited2 <- df[df$like == "1", ]
odds2 <- sum(limited2$PositiveSentiment == "1") / sum(limited2$PositiveSentiment == "0")
print(odds2)
