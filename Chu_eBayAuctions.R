## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------------------------
library(rmarkdown)
library(rpart)
library(rpart.plot)
library(caret)


## -----------------------------------------------------------------------------------------------------------
auc.data<- read.csv("eBayAuctions.csv")


## -----------------------------------------------------------------------------------------------------------
# Preview the head and summary of the dataset
head(auc.data)
summary(auc.data)
dim(auc.data)
str(auc.data)


## -----------------------------------------------------------------------------------------------------------
auc.data$Competitive. <- ifelse(auc.data$Competitive. == 1 , "Competitive", "Non-Competitive")


## -----------------------------------------------------------------------------------------------------------
auc.data$Category <- as.factor(auc.data$Category)
auc.data$currency <- as.factor(auc.data$currency)
auc.data$Duration <- as.factor(auc.data$Duration)
auc.data$endDay <- as.factor(auc.data$endDay)
str(auc.data)


## -----------------------------------------------------------------------------------------------------------
#60% training, 40% testing
set.seed(100)
auc.data <- data.frame(auc.data)
rows <- c(1:nrow(auc.data))
split <- sample(rows,size = (nrow(auc.data)*0.60))
train <- auc.data[split,]
test <- auc.data[-split,]
nrow(train)
head(train)
nrow(test)
head(test)


## -----------------------------------------------------------------------------------------------------------
train.tree1 <- rpart(Competitive. ~  Category + currency + Duration + endDay + sellerRating + ClosePrice + OpenPrice, data = train, method = "class",  maxdepth = 7, minbucket = 50)
rpart.plot(train.tree1)
summary(train.tree1)


## -----------------------------------------------------------------------------------------------------------
pruned.train.tree1 <- prune(train.tree1, cp = train.tree1$cptable[which.min(train.tree1$cptable[,"xerror"]),"CP"])
length(pruned.train.tree1$frame$var[pruned.train.tree1$frame$var == "<leaf>"])
prp(pruned.train.tree1, type = 2, extra = 1, split.font = 1, varlen = -10)  


## -----------------------------------------------------------------------------------------------------------
train.tree2 <- rpart(Competitive. ~  sellerRating + OpenPrice, data = train, method = "class",  maxdepth = 7, minbucket = 50)
rpart.plot(train.tree2)
summary(train.tree2)


## -----------------------------------------------------------------------------------------------------------
pruned.train.tree2 <- prune(train.tree2, cp = train.tree2$cptable[which.min(train.tree2$cptable[,"xerror"]),"CP"])
length(pruned.train.tree2$frame$var[pruned.train.tree2$frame$var == "<leaf>"])
prp(pruned.train.tree2, type = 2, extra = 1, split.font = 1, varlen = -10)  


## -----------------------------------------------------------------------------------------------------------
pred1 <- predict(pruned.train.tree1, train, type = 'class')
plot(train$sellerRating, train$OpenPrice, main = "Training Dataset including all predictors", col = ifelse(pred1 == "Competitive","red","blue"))
mean(train$Competitive.!=pred1)
confusionMatrix(pred1, as.factor(train$Competitive.)) 

pred2 <- predict(pruned.train.tree1, test, type = 'class')
plot(test$sellerRating, test$OpenPrice, main = "Testing Dataset including all predictors", col = ifelse(pred2 == "Competitive","red","blue"))
mean(test$Competitive.!=pred2)
confusionMatrix(pred2, as.factor(test$Competitive.)) 

pred3 <- predict(pruned.train.tree2, train, type = 'class')
plot(train$sellerRating, train$OpenPrice, main = "Training Dataset with significant predictors", col = ifelse(pred3 == "Competitive","red","blue"))
mean(train$Competitive.!=pred3)
confusionMatrix(pred3, as.factor(train$Competitive.)) 

pred4 <- predict(pruned.train.tree2, test, type = 'class')
plot(test$sellerRating, test$OpenPrice, main = "Testing Dataset with significant predictors", col = ifelse(pred4 == "Competitive","red","blue"))
mean(test$Competitive.!=pred4)
confusionMatrix(pred4, as.factor(test$Competitive.)) 

