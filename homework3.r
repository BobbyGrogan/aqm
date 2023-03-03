# HOMEWORK 3
# set directory and get data from CSV file, read data into a dataframe
# setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
df <- read.csv("Data/SandwichAnts.csv")

df$X <- NULL
df$Trail <- NULL
df$Bread <- as.factor(df$Bread)
df$Filling <- as.factor(df$Filling)
df$Butter <- as.factor(df$Butter)
df$ManyAnts <- df$Ants > 50

set.seed(1234)
n <- nrow(df)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- df[trainingcases, ]
test <- df[-trainingcases, ]

model1 <- glm(ManyAnts ~ Filling, data=training, family=binomial()) 
#summary(model1) 

predictions <- predict(model1, test) 
predictionsTF <- predictions > 0.5
observations <- test$ManyAnts
confusion <- table(predictionsTF, observations) 

TP <- confusion[2, 2]
FP <- confusion[1, 2]
TN <- confusion[1, 1]
FN <- confusion[2, 1]

accuracy <- (TP + TN) / (TP + FP + TN + FN)

#error_rate <- sum(predictionsTF != observations)/nrow(test) print(error_rate) 
sensitivity <- TP / (TP + FN)
spesificity <- TN / (TN + FP)
#print(sensitivity) 
#print(spesificity)

model2 <- glm(ManyAnts ~ Butter + Filling + Bread, data=training, family=binomial())
model2 <- step(model2)
summary(model2)

predictions <- predict(model2, test)
predictionsTF <- predictions > 0.5
observations <- test$ManyAnts
confusion <- table(predictionsTF, observations)
print(confusion)

TP <- confusion[2, 2]
FP <- confusion[1, 2]
TN <- confusion[1, 1]
FN <- confusion[2, 1]
  
accuracy <- (TP + TN) / (TP + FP + TN + FN)
print(accuracy)

library(ggplot2)
X11()

ggplot(df, aes(x = Ants, y = ManyAnts, color = factor(Ants))) + geom_point()

Sys.sleep(5)
