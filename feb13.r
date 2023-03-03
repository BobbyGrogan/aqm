# Loading data
dfBH <- read.csv("Data/BostonHousing.csv")

# Data mangament
dfBH$CHAS <- as.logical(dfBH$CHAS)
dfBH$RAD <- as.factor(dfBH$RAD)
dfBH$ISHIGHVAL2 <- dfBH$ISHIGHVAL
dfBH$ISHIGHVAL2 <- as.logical(dfBH$ISHIGHVAL2) 
dfBH$MEDV <- NULL

# Partition
set.seed(1234)
n <- nrow(dfBH)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- dfBH[trainingcases, ]
test <- dfBH[-trainingcases, ]

# Graphing
library(ggplot2)
X11()

ggplot(dfBH, aes(x=RM, y=ISHIGHVAL)) + geom_point()
ggplot(dfBH, aes(x=RM, y=ISHIGHVAL2)) + geom_point()

# Model

model = glm(ISHIGHVAL2 ~ ., data=training, family=binomial())
model = step(model)

model2 = glm(ISHIGHVAL2 ~ RM, data=training, family=binomial())
model2 = step(model2)
summary(model2)

final = exp(-14.6066+(2.4053^2)))/(1+exp(-14.6066+(2.4053^2))))
