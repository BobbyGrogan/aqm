# CLASS ON JANUARY 23RD

# call data and make it a variable
dfBostonHousing <- read.csv("Data/BostonHousing.csv", header=TRUE)

# changes info in column to different operator
dfBostonHousing$CHAS <- as.logical(dfBostonHousing$CHAS)
dfBostonHousing$ISHIGHVAL <- as.logical(dfBostonHousing$ISHIGHVAL)

# pulls up GUI of the dataframe
#View(dfBostonHousing)

# gives the column names , the 't' part puts numbers on top of names
#t(names(dfBostonHousing))

# gives a certain row, column
#print(dfBostonHousing[2,14])

# new dataframe with only data from columns 1 through 5
df2 <- dfBostonHousing[,1:5]

# new dataframe with columns CHAS and RM only
df3 <- dfBostonHousing[,c("CHAS","RM")]

# changes data in column, multiplied 'CRIM' column by 2
df4 <- 2 * dfBostonHousing$CRIM

# gives summary of what is in data
#summary(dfBostonHousing$CRIM)

# pulls the data structure, describes operator type, etc.
str(dfBostonHousing)
