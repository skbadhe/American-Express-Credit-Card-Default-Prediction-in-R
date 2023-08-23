#Shubham Badhe
#input data
InputData.df <- read.csv("C:/Users/skb24/Downloads/Study/Input Data.csv", header=TRUE, stringsAsFactors=TRUE)
TrainLabels.df <- read.csv("C:/Users/skb24/Downloads/Study/train_labels.csv", header=TRUE, stringsAsFactors=TRUE)

colnames(TrainLabels.df)
colnames(InputData.df)

merged.df <- merge.default(TrainLabels.df, InputData.df)

colnames(merged.df)
sort(colnames(merged.df))

#finding correlation between variables
library(dplyr)

BalanceVars <- select(merged.df,starts_with("B"))

cor(BalanceVars)

#convert target to factors with 2 levels 0,1
merged.df$target <- as.factor(merged.df$target)

#convert date column from factor to date type
#merged.df$S_2 <- as.Date(merged.df$S_2)

#remove cust id
merged.df <- merged.df[-c(1)]
#remove date
merged.df <- merged.df[-c(3)]

str(merged.df)

#replace na values with column mean -- working method
data2 <- merged.df
for(i in 1:ncol(merged.df)) {
  data2[ , i][is.na(data2[ , i])] <- mean(data2[ , i], na.rm = TRUE)
}
  
#random forest
library(randomForest)
library(datasets)
library(caret)

set.seed(222)
ind <- sample(2, nrow(data2), replace = TRUE, prob = c(0.1, 0.9))
train <- data2[ind==1,]
test <- data2[ind==2,]

rf.model <- randomForest(target~., data=train, proximity=TRUE, na.action=na.roughfix)

print(rf.model)

#prediction and confusion matrix train data
p1 <- predict(rf.model,train)
confusionMatrix(p1, train$target)

#prediction and confusion matrix test data
p2 <- predict(rf.model,test)
confusionMatrix(p2, test$target)

#plot model on graph
plot(rf.model)
