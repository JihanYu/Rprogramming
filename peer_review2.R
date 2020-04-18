#Practical Machine Learning Course Project

setwd("~/R/Practical Machine Learning/Course Project")

training <- read.csv("pml-training.csv", header=TRUE)

testing <- read.csv("pml-testing.csv", header=TRUE)

library(caret); library(ggplot2)



#Exploratory data analysis

dim(training)

dim(testing)

str(training)

str(training$classe)

summary(training$classe)

g <- ggplot(data=training, aes(x = classe))

g + geom_bar(aes(fill = user_name))



#Remove variables with near zero variance

nzv <- nearZeroVar(training)

training_nzv <- training[, -nzv]

dim(training_nzv)

testing_nzv <- testing[, -nzv]

dim(testing_nzv)



#Remove variables that are mostly NA

all_na <- sapply(training_nzv, function(x) mean(is.na(x))) > 0.95

all_na

#colindex <- colSums(is.na(training_nzv))/nrow(training_nzv) < 0.95

train <- training_nzv[, all_na == FALSE]

test <- testing_nzv[, all_na == FALSE]

colnames(trainset)

dim(trainset)

str(trainset)

str(trainset$classe)



#Remove first 7 variables

train <- train[, -c(1:7)]

test <- test[, -c(1:7)]



#Create training and testing set from original training data

set.seed(1234)

inTrain <- createDataPartition(train$classe, p=0.6, list=FALSE)

trainset <- train[inTrain,]

testset <- train[-inTrain,]



#Decision tree

library(rpart.plot)

modfit <- train(classe ~., data=trainset, method="rpart")

modfit$finalModel

DT_prediction <- predict(modfit, testset)

confusionMatrix(DT_prediction, testset$classe)

rpart.plot(modfit$finalModel, roundint=FALSE)



#Random forest

RF_modfit <- train(classe ~ ., data = trainset, method = "rf", ntree = 10)

RF_prediction <- predict(RF_modfit, testset)

RF_pred_conf <- confusionMatrix(RF_prediction, testset$classe)

RF_pred_conf



predict(RF_modfit, test)