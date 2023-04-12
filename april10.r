# BOBBY AND ETHAN

# set the working directory and get the csv
setwd("C://Users/rgrogan1/Desktop/AQM2000/Data")
df =  read.csv("DairyPurchase.csv")

# run a k means model which get aims differentiate 4 different clusters from df
# k mean calculates the distance between each data point and a centroid to assign it to a cluster
model = kmeans(df,4)

# gives the center of each cluster for each variable
model$centers
# give the size of each cluster
model$size

# get the library to get preProcess function
library(caret)

# create the process to transform to z-scores
standardizer = preProcess(df,method=c("center","scale"))

# make a dataframe of z-scores
df2 = predict(standardizer,df)

# run a k means model which get aims differentiate 4 different clusters from the z-scores dataframe
model2 = kmeans(df2,4)

# gives the center of each cluster for each variable
model2$centers
# give the size of each cluster
model2$size

# get the babson analytics file to use its functions such as elbow chart
source("C:/Users/rgrogan1/Desktop/BabsonAnalyticsC.R")

# displays a chart where the 'elbow' represents the ideal number of clusters, we find optimal is 3
elbowChart(df)

# create new dataframe from Utilities.csv
df3 = read.csv("Data/Utilities.csv")

# do not need to remove this variable despite it being it is not numerical and is needed for ggplot
#df3$Company = NULL

# uneeded line of code which loads in caret library again
library(caret)

# create the process to transform to z-scores
standardizer = preProcess(df3 ,method=c("center","scale"))

# make a dataframe of z-scores
df3 = predict(standardizer,df3)

# finds distance between rows and columns
d = dist(df3)

# show d 
d

# run hierarchical cluster analysis on the mean distance between elements of each cluster
# hierarchical cluster starts with a cluster for each data point and then merges them
model = hclust(d, method = "average")

# plot previous model
plot(model, labels = df3$Company)

# run hierarchical cluster analysis on the minimum distance between elements of each cluster
model = hclust(d, method = "single")

# plot previous model
plot(model, labels = df3$Company)

# run hierarchical cluster analysis on the maximum distance between elements of each cluster
model = hclust(d, method = "complete")

# plot previous model
plot(model, labels = df3$Company)

# get ggplot library for special plotting
library("ggplot2")

# create point graph with sales as X and cost as Y and company as label
ggplot(df3, aes(df3$Sales, df3$Fuel_Cost, label = df3$Company)) +    # ggplot2 plot with labels
  geom_point()+ geom_text(aes(label = df3$Company), hjust = - 0.5)

