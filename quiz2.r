# set directory and get data 
setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
dfBackpack <- read.csv("Data/Backpack.csv")

# manage the data, change data types where necessary
dfBackpack$BackProblems <- as.logical(dfBackpack$BackProblems)
dfBackpack$Sex <- as.factor(dfBackpack$Sex)
dfBackpack$Status <- as.factor(dfBackpack$Status)
dfBackpack$Major <- as.factor(dfBackpack$Major)

# set seed, partition the data, gets the number of rows, then sets training size to 60% the number of rows, and pick random sample
set.seed(1234)
n <- nrow(dfBackpack)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- dfBackpack[trainingcases, ]
test <- dfBackpack[-trainingcases, ]

# create model with BackpackWeight as Y-variable and BodyWeight as X-variable, then give summary of model
model <- lm(BackpackWeight ~ BodyWeight, data = training)
summary(model)

# the residuals are wide ranging
# they have a range of 31 pounds which is only slightly smaller than the entire range of backpack weight which is 33 pounds 
# having such wide ranging residuals suggest a relationship is not strong

# the line of least squares is f(x) = 0.04138x + 5.66762

# the p value is greater than 0.1 which is fairly high
# this suggests that there is no statistical relationship between a persons body weight and their backpack weight
