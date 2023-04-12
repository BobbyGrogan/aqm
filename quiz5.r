#setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
library(ggplot2)
library(caret)
source("Data/BabsonAnalytics.R")

df = read.csv("Data/Hitters2.csv")

# these columns are non numerical values so they should be eliminated
df$League <- NULL
df$Division <- NULL
df$NewLeague <- NULL
# this column has many missing data points
df$Salary <- NULL
# this is just an index column
df$X <- NULL

# puts all values into z scores
columns <- df[,1] + df[,2] + df[,4:16]
process <- preProcess(columns,c("center", "scale"))
dfstandard <- predict(process, df)

# puts all values into normalization
process2 <- preProcess(columns,c("range"))
dfnormal <- predict(process2, df)

set.seed(10)

# standardization partition 
nstandard <- nrow(dfstandard)
sizestandard <- (nstandard * 0.6)
trainingcasestandard <- sample(nstandard, sizestandard)
trainingstandard <- df[trainingcasestandard, ]
teststandard <- df[-trainingcasestandard, ]

# normalization partition
nnormal <- nrow(dfnormal)
sizenormal <- (nnormal * 0.6)
trainingcasesnormal <- sample(nnormal, sizenormal)
trainingnormal <- df[trainingcasesnormal, ]
testnormal <- df[-trainingcasesnormal, ]

# get appropriate K value
knnCrossVal(HmRun ~ ., data=trainingstandard)
# 13 is the K value for the standardization model
knnCrossVal(HmRun ~ ., data=trainingnormal)
# 6 is the K value for the normalization model

# create the model
modelnormal <- knnreg(HmRun ~ ., data=trainingnormal, k=6)
modelstandard <- knnreg(HmRun ~ ., data=trainingstandard, k=3)

# from the models and get mapes, rmses
predictionsnormal <- predict(modelnormal, testnormal)
observationsnormal <- testnormal$HmRun
errorsnormal <- observationsnormal - predictionsnormal
mapenormal <- mean(abs(errorsnormal/observationsnormal))
rmsenormal <- sqrt(mean(errorsnormal^2))

predictionsstandard <- predict(modelstandard, teststandard)
observationsstandard <- teststandard$HmRun
errorsstandard <- observationsstandard - predictionsstandard
mapestandard <- mean(abs(errorsstandard/observationsstandard))
rmsestandard <- sqrt(mean(errorsstandard^2))

# since the RMSE and MAPE are lower for the normalization model, I conclude that the normaliztion model is a better predicor of home runs
str(df)

print(mapenormal)
print(rmsenormal)
print(mapestandard)
print(rmsestandard)
