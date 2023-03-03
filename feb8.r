# set directory and get data
#setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
dfCA <- read.csv("Data/CASchools.csv")

# manage the data
dfCA$county <- NULL
dfCA$grades <- as.logical(dfCA$grades)
dfCA$school <- NULL
dfCA$district <- NULL

# partition the data, gets the number of rows, then sets training size to 60% the number of rows, and pick random sample
set.seed(12)
n <- nrow(dfCA)
trainingsize <- round(n * 0.6)
trainingcases <- sample(n, trainingsize)
training <- dfCA[trainingcases, ]
test <- dfCA[-trainingcases, ]

# all parameters
model1 <- lm(math ~ ., data = training)
#summary(model1)

# runs the subsets and finds the best ones
model2 <- step(model1)
# gives summary of model5 with best subsets
#summary(model2)

# question 6: the best subset retained X, expenditure, income, english, income, and read
# I disagree that the variable X would have any influence

predictions = predict(model2, test)
observations = test$math
errors = observations - predictions
#X11()
#hist(errors)

# create a scatter plot
#library(ggplot2)
#ggplot(test, aes(y = errors, x = observations, color = errors)) + geom_point() + labs(x = "Fit_Test", y = "Residuals_Test", title = "Scatterplot") + scale_color_gradient(low = "blue", high = "green")

# 9: the histogram has an approximately normal distribution, there good evidence for linear regression
# 9: the scatter plot appears to be random which implies that the model is good for prediction 

mape <- mean(abs(errors/observations))
rmse <- sqrt(mean(errors^2))
prediction_benchmark <- mean(training$math)
errors_benchmark <- observations - prediction_benchmark
rmse_benchmark <- sqrt(mean(errors_benchmark^2))
mape_benchmark <- mean(abs(errors_benchmark/observations))

#print(rmse_benchmark)
#print(mape_benchmark)
#print(rmse)
#print(mape)

# 11: our model did perform better than the benchmark becuase it had lower MAPE and RMSE

# 12: I think the model1 will be less accurate since model1 has more variables than model2

predictions = predict(model1, test)
observations = test$math
errors = observations - predictions
#X11()
#hist(errors)

# create a scatter plot
#library(ggplot2)
#ggplot(test, aes(y = errors, x = observations, color = errors)) + geom_point() + labs(x = "Fi>

# 9 repeated: the histogram has an approximately normal distribution, there good evidence for linear re>
# 9 reapeated: the scatter plot appears to be random which implies that the model is good for prediction

mape <- mean(abs(errors/observations))
rmse <- sqrt(mean(errors^2))
prediction_benchmark <- mean(training$math)
errors_benchmark <- observations - prediction_benchmark
rmse_benchmark <- sqrt(mean(errors_benchmark^2))
mape_benchmark <- mean(abs(errors_benchmark/observations))

#print(prediction_benchmark)
#print(errors_benchmark)
print(rmse_benchmark)
print(mape_benchmark)
print(rmse)
print(mape)

# 13: model1 is less accurate since it uses lots of variables which are not relavant, it has a higher RMSE and MAPE which means on average the residuals are higher 
# 13: model2 is the most accurate since it uses only relevant variables, it has the lowest average error (RMSE and MAPE) of models tested here
# 13: the benchmark is the least accurate since it uses no model
# 13: my answer does suport my thinking since using some model is better than no model, and using a best subsets model is the most accurate

# 14: I could try to approve the model by removing outliers above a certain Z-score and by removing some less accurate variables

model_test = lm(math ~ expenditure + income + read, data = training)
predictions = predict(model_test, test)
observations = test$math
errors = observations - predictions

mape <- mean(abs(errors/observations))
rmse <- sqrt(mean(errors^2))
prediction_benchmark <- mean(training$math)
errors_benchmark <- observations - prediction_benchmark
rmse_benchmark <- sqrt(mean(errors_benchmark^2))
mape_benchmark <- mean(abs(errors_benchmark/observations))

#print(prediction_benchmark)
#print(errors_benchmark)
print(rmse_benchmark)
print(mape_benchmark)
print(rmse)
print(mape)

# 15: the models that I tested were less accurate