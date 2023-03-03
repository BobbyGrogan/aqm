# using the dataset alcohol, get the dataset
dfAlc = read.csv("Data/alcohol.csv")

# gets the library used for graphing
library(ggplot2)

# change logical to factor
dfAlc$abuse <- as.factor(dfAlc$abuse)

# change the name of factor variables
levels(dfAlc$abuse) <- c("Yes", "No")

# creates a stacked bar graph where those who abuse alcohol are in blue, those that do not are in orange
myplot <- ggplot(dfAlc, aes(x=age, fill=abuse)) + geom_bar() +
	xlab("Person Age") + ylab("Number of people")

# changes the title of the legend
myplot <- myplot + guides(fill=guide_legend(title="Abuse?"))

print(myplot)

# creates a jitter plot and box plot, also changed xlabel, ylabel, and some colors
myplot2 <- ggplot(dfAlc, aes(x=age, y=abuse)) + 
	geom_jitter(shape=100, position=position_jitter(0.5), color="#ff0000") + 
	geom_boxplot(color='#33ff44') +
	xlab("Person Age") + ylab("Does Abuse Alcohol?")

print(myplot2)
