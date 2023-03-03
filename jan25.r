# CLASS ON JANUARY 25TH

# upload the data
dfBikes = read.csv("Data/updated_bike_share.csv")

# change columns from integer when they should be factor (categorical) or logical
dfBikes$season <- as.factor(dfBikes$season)
dfBikes$month <- as.factor(dfBikes$month)
dfBikes$weather <- as.factor(dfBikes$weather)
dfBikes$holiday <- as.logical(dfBikes$holiday)
dfBikes$workingday <- as.logical(dfBikes$workingday)
dfBikes$weekday <- as.factor(dfBikes$weekday)

# gets the library used for graphing
library(ggplot2)

# ONLY for when from command line, gets graphical display, not from class
X11()

# creates a line graph using the dfBikes dataset and declares the x and y variables
#ggplot(dfBikes, aes(x=hoursSinceStart, y=total))

# actually graphs a line on the graph
#ggplot(dfBikes, aes(x=hoursSinceStart, y=total)) + geom_line()

# adss a limit to the x axis
#ggplot(dfBikes, aes(x=hoursSinceStart, y=total)) + geom_line() + xlim(0, 500)

# adds a limit to the y axis
#ggplot(dfBikes, aes(x=hoursSinceStart, y=total)) + geom_line() + xlim(0, 500) + ylim(0, 250)

# creates a simple bar graph with seasons on X axis and their frequency on Y
#ggplot(dfBikes, aes(x=season)) + geom_bar()

# creates a bar graph where stat="identity" tells it to count value instead of num of cases
#ggplot(dfBikes, aes(x=season, y=total)) + geom_bar(stat="identity")

# creates a histogram but does not work becuase improper bin width
#ggplot(dfBikes, aes(x=total)) + geom_histogram()

# bin number should be the closest integer to the square root of the size of the dataset
#ggplot(dfBikes, aes(x=total)) + geom_histogram(bins=104)

# creates a heatmap with weekday and month on axes
#ggplot(dfBikes, aes(x=weekday, y=month, fill=registered)) + geom_tile(stat="sum")
myplot <- ggplot(dfBikes, aes(x=weekday, y=month, fill=casual)) + geom_tile(stat="sum")

# stacked bar chart
#ggplot(dfBikes, aes(x=month, y=registered, fill=workingday)) + geom_bar(stat="identity")
#ggplot(dfBikes, aes(x=month, y=casual, fill=workingday)) + geom_bar(stat="identity")

# ONLY from command line; saves plot as a png; png can be replaced with pdf, not from class 
#png("myplot.png")
#print(myplot)
#dev.off()


