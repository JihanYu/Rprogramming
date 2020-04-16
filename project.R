workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\DS08 Practical machine learning"
setwd(workingpath)

pml.training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header=TRUE)
pml.testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=TRUE)

# predict the manner of exercise : "classe"

library(caret);  library(ggplot2);  library(rattle)

training <- pml.training[, c(8:160)]
factor.id <- which(sapply(training, is.factor));  factor.n <- length(factor.id)

for(i in factor.id[-factor.n]){
	training[, i] <- as.character(training[, i])
	training[, i] <- as.numeric(training[, i])
}
training[is.na(training)] <- 0


inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
training.in <- training[inTrain,];  testing.in <- training[-inTrain,]
dim(training.in);  dim(testing.id)

modFit.rpart <- train(classe ~ ., method="rpart", data=training.in)
print(modFit.rpart)

fancyRpartPlot(modFit.rpart$finalModel)

pr.train.rpart <- predict(modFit.rpart, newdata=training.in)
confusionMatrix(training.in$classe, pr.train.rpart)

pr.test.rpart <- predict(modFit.rpart, newdata=testing.in)
confusionMatrix(testing.in$classe, pr.test.rpart)


modFit.rf <- train(classe ~ ., method="rf", data=training.in, prox=TRUE)
print(modFit.rf)
# Error 로 수행 불가
#getTree(modFit.rf$finalModel, k=5)
#
#pr.train.rf <- predict(modFit.rf, newdata=training.in)
#confusionMatrix(training.in$classe, pr.train.rf)

#pr.test.rf <- predict(modFit.rf, newdata=testing.in)
#confusionMatrix(testing.in$classe, pr.test.rf)

modFit.lda <- train(classe ~ ., method="lda", data=training.in)
# Error 로 불가
# print(modFit.lda)
#pr.train.lda <- predict(modFit.lda, newdata=training.in)
#confusionMatrix(training.in$classe, pr.train.lda)
#pr.test.lda <- predict(modFit.lda, newdata=testing.in)
#confusionMatrix(testing.in$classe, pr.test.lda)

modFit.nb <- train(classe ~ ., method="nb", data=training.in)
print(modFit.nb)
pr.train.nb <- predict(modFit.nb, newdata=training.in)
confusionMatrix(training.in$classe, pr.train.nb)
pr.test.nb <- predict(modFit.nb, newdata=testing.in)
confusionMatrix(testing.in$classe, pr.test.nb)



testing <- pml.testing[, c(8:160)]
factor.id <- which(sapply(testing, is.factor));  factor.n <- length(factor.id)

for(i in factor.id[-factor.n]){
	testing[, i] <- as.character(testing[, i])
	testing[, i] <- as.numeric(testing[, i])
}
testing[is.na(testing)] <- 0

predict(, newdata=testing)





preObj <- preProcess(tr.red[, -factor.n], method="knnImpute")
res <- predict(preObj, tr.red[, -factor.n])

kMeans1 <- kmeans(subset(tr.red, select=-c(classe)), centers=5)
tr.red$clusters <- as.factor(kMeans1$cluster)
table(tr.red$classe, tr.red$clusters)

modFit <- train(clusters ~ ., data=subset(tr.red, select=-c(classe)), method="rpart")
table(predict(modFit, tr.red), tr.red$classe)

testClusterPred <- predict(modFit, pml.testing)


kMeans1 <- kmeans(subset(training, select=-c(classe)), centers=3)



a <- training[, factor.id];  b <- sapply(a, class)
dim(a)

nc <- ncol(a) -1
for(i in c(1:nc)){
	div.id <- which(a[,i]==div.ch)
	a[div.id,i] <- NA
}


kMeans1 <- kmeans(subset(training, select=-c(classe)), centers=3)

# names(pml.training)[c(1:7,160)]
# c(1:7, 160)
#[1] "X"                    "user_name"            "raw_timestamp_part_1" "raw_timestamp_part_2"
#[5] "cvtd_timestamp"       "new_window"           "num_window"           "classe" 
