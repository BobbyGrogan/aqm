df = read.csv("eBayAuctions.csv")
df2 = read.csv("crTreeLesson.csv")

df$Category = as.factor(df$Category)
df$Currency = as.factor(df$Currency)
df$EndDay = NULL
df$Competitive = as.factor(df$Competitive)
df$ClosePrice = NULL

set.seed(12)
n = nrow(df)
trainingsize = round(n * 0.6)
trainingcases = sample(n, traningsize)
training = df[trainingcases, ]
test = df[-trainingcases, ]

model = rpart(Competitive ~ ., data = training)
rpart.plot(model)

rpart.plot(model, tweak = 1.1, varlen = 4, faclen = 4)
predictions = predict(model, test)
View(predictions)
