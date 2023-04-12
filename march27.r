#importing the libraries needed
# These specific libraries are used for decision tree carts
# If library doesnt work, pls install packages
source("Data/BabsonAnalytics.R")
library(rpart)
#install.packages("rpart.plot_3.1.1.tar.gz", repos = NULL, type="source")
library(rpart.plot)
X11()

# Imports the required files and makes them into data 
# frames 1 and 2 (only works on csv files)
df = read.csv("Data/eBayAuctions.csv")
#df2 = read.csv("crTreeLesson.csv")

#EndDay and ClosePrice are not relevant to
# our tests today so they are nulled
df$Category = as.factor(df$Category)
df$Currency = as.factor(df$Currency)
df$EndDay = NULL
df$Competitive = as.factor((df$Competitive))
df$ClosePrice = NULL

#Partition Data (do this when your data set 
# is large)
set.seed(12)
N = nrow(df)
trainingSize = round(N*.6)
trainingCases = sample(N, trainingSize)
training = df[trainingCases,]
test = df[-trainingCases,]

# Creating a model of the decision tree
model = rpart(Competitive ~ ., data = training)
rpart.plot(model)# set the size to US legal

# Makes graph readable but needs more research on what
# this line is actually doing
rpart.plot(model,tweak = 1.1,varlen = 4,faclen = 4)
#this was something I found online 

#KPI
predictions = predict(model, test)# not what we want 
View(predictions)#not what we want, this says we are 9% not competivive and 91% we are competivive for obs 2 


predictions = predict(model, test, type = "class") # this type = "class sayes dont give me the numbers give me the factors level back 
observations  = test$Competitive
#View(predictions)

error_rate = sum(predictions != observations)/nrow(test)
error_bench = benchmarkErrorRate(training$Competitive, test$Competitive)


#    minSplit = is the node big enough to split, typically something like 2% of the total number of records
#    minBucket = when you split, the split should be big enough (on both side) of the split. Something like 1% of the total number of records 
#    Complexity parameter = CP = does the split improve the model (many factors go into this)
stopping_rules = rpart.control(minsplit = 200, minbucket = 100, cp = 0)#make 2,1,0 to see what we did in class for model2 
model = rpart(Competitive ~ ., data = training, control = stopping_rules)
rpart.plot(model)
Sys.sleep(120)

predictions_overfit = predict(model, test, type = "class")
error_rate_overfit = sum(predictions_overfit != observations)/nrow(test)


model = easyPrune(model)
rpart.plot(model)


predictions_pruned = predict(model, test, type = "class")
error_rate_pruned = sum(predictions_pruned != observations)/nrow(test)


#model2 = rpart(success ~ ., data = df2,control = stopping_rules )
#rpart.plot(model2)

 
