# HOMEWORK 1
# set directory and get data from CSV file, read data into a dataframe
setwd("C:/Users/rgrogan1/Desktop/AQM2000/Data")
dfSA <- read.csv("Data/SandwichAnts.csv")

# remove all columns except Butter and Ants
dfSA$X <- NULL
dfSA$Trial <- NULL
dfSA$Bread <- NULL
dfSA$Filling <- NULL

# since Butter column is chr, data is not appropriate, so change Butter column to factor
dfSA$Butter <- as.factor(dfSA$Butter)

# get ggplot library 
library(ggplot2)

# creates a bar plot which counts total ants for butter and not butter
ggplot(dfSA, aes(x=Butter, y=Ants)) + geom_bar(stat="identity")

# creates a jitter plot which displays what each ant count was and whether the sandwhich was buttered or not
ggplot(dfSA, aes(x=Ants, y=Butter)) + geom_jitter(shape=10, position=position_jitter(0.2))

# get the mean and standard deviation of the Ants data
mean_ants <- mean(dfSA$Ants)
standard_deviation_ants <- sd(dfSA$Ants)

# create a second df of just ants, then calculate all z-scores for those items, then save all scores as column in original dataframe
dfSA2 <- dfSA[ ,c("Ants")]
zscores <- (dfSA2 - mean(dfSA2))/sd(dfSA2)
dfSA$scores <- zscores
