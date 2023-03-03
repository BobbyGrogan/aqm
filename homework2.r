# HOMEWORK 2
# set directory and get data from CSV file, read data into a dataframe
# setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
df <- read.csv("Data/HousesNY.csv")

# modify the columns, all others are in proper data type
df$X = NULL

# find mean of dataset
print(mean(df$Price))

# find max of dataset
print(max(df$Lot))

X11()

# create scatterplot matrix
pairs(df)

# make box plots for each variable to find outliers
boxplot(df$Price, horizontal = TRUE)
boxplot(df$Beds, horizontal = TRUE)
# there is one outlier of beds with 6 bedrooms
boxplot(df$Baths, horizontal = TRUE)
# there are two outliers of bathrooms with 3 and 3.5 bathrooms
boxplot(df$Size, horizontal = TRUE)
boxplot(df$Lot, horizontal = TRUE)
# there are two outliers of lot size at 2.5 and 3.5 acres

# creat linear model
set.seed(1234)
n <- nrow(df)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- df[trainingcases, ]
test <- df[-trainingcases, ]

model <- lm(Price ~ Size, data=training)

summary(model)

predictions <- predict(model, test)
observations <- test$Price
errors <- observations - predictions

# get rmse
rmse <- sqrt(mean(errors^2))
print(rmse)

# create model with all variables
model2 <- lm(Price ~ ., data=training)

summary(model2)

predictions <- predict(model2, test)
observations <- test$Price
errors <- observations - predictions
  
rmse <- sqrt(mean(errors^2))
print(rmse)

# create a best subset model
model3 <- lm(Price ~ ., data=training)
model3 <- step(model3)
  
summary(model3)
  
predictions <- predict(model3, test)
observations <- test$Price
errors <- observations - predictions
  
rmse <- sqrt(mean(errors^2))
print(rmse)

-7.478 + 13.849*3 + 34.314*1.5 + 15.437*0.5
