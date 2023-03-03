# set directory and get data
#setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
dfToyota <- read.csv("Data/ToyotaCorolla.csv")

# manage the data
dfToyota$Fuel_Type <- as.factor(dfToyota$Fuel_Type)
dfToyota$Met_Color <- as.logical(dfToyota$Met_Color)
dfToyota$Automatic <- as.logical(dfToyota$Automatic)

# delete a certain column of data
dfToyota$Model <- NULL

# partition the data, gets the number of rows, then sets training size to 60% the number of rows, and pick random sample
set.seed(1234)
n <- nrow(dfToyota)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- dfToyota[trainingcases, ]
test <- dfToyota[-trainingcases, ]

# create model with Price as Y-variable and Age as X-variable, then give summary of model
model <- lm(Price ~ Age, data = training)
#print(model)
#summary(model)

# NOT FROM CLASS, create plot of linear model
#library(ggplot2)
#X11()
#my <- ggplot(dfToyota, aes(x = Age, y = Price)) + geom_point() + stat_smooth(method = "lm", col = "red")
#print(my)
#Sys.sleep(10)

# create second model with multiple X variables
model2 <- lm(Price ~ Age + HP, data = training) 
#print(model2)
#summary(model2)

# all parameters
model3 <- lm(Price ~ ., data = training)
#summary(model3)

# multiple parameters
model4 <- lm(Price ~ Age + KM + Fuel_Type + HP + cc + Weight, data = training)
#summary(model4)

# make some predictions, test what the data actually is, calculate difference
predictions <- predict(model3, test)
observations <- test$Price
errors <- observations - predictions

# create a quick histogram of the errors
hist(errors)

# RMSE root mean squared error, square the errors to get distance (remove negatives)
rmse <- sqrt(mean(errors^2))
#print(rmse)

# MAPE mean average percentage error
mape <- mean(abs(errors/observations))
#print(mape)
#print(mean(test$Price))

# rebuild model with better parameters, step finds the best subsets for a model (lowest p-value)
model5 <- lm(Price ~ ., data = training)
# runs the subsets and finds the best ones
model5 <- step(model5)
# gives summary of model5 with best subsets
#summary(model5)

# evaluate perfomance to see if rmse and mape are good
prediction_benchmark <- mean(training$Price)
errors_benchmark <- observations - prediction_benchmark
rmse_benchmark <- sqrt(mean(errors_benchmark^2))
mape_benchmark <- mean(abs(errors_benchmark/observations))
#print(prediction_benchmark)
#print(errors_benchmark)
#print(rmse_benchmark)
#print(mape_benchmark)

library(ggplot2)
# create new columns in data
test$errors <- errors
test$observations <- observations

X11()
ggplot(test, aes(y = errors, x = observations, color = errors)) + geom_point() + labs(x = "Fit_Test", y = "Residuals_Test", title = "Scatterplot") + scale_color_gradient(low = "blue", high = "green")
Sys.sleep(10)

