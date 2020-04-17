workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\project" # office
# workingpath <- "C:\\Users\\pc\\Desktop\\Jihan"  # home
setwd(workingpath)

##### Load data & pre-processing #####
pml.training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header=TRUE)
pml.testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=TRUE)

library(caret);  library(ggplot2);  library(rattle)
training <- pml.training[, c(8:160)]    # Fitbit data only

# convert variable type : factor to numeric 
factor.id <- which(sapply(training, is.factor));  factor.n <- length(factor.id)
for(i in factor.id[-factor.n]){
  training[, i] <- as.character(training[, i])
  training[, i] <- as.numeric(training[, i])
}

##### Imputation #####
# apply all NA values to zero (0)
# training[is.na(training)] <- 0

# select variables which have NA less than half
n.na <- function(vec){
	return ( sum(is.na(vec), na.rm=TRUE) )
}
n.row <- nrow(training)
na.id <- which( sapply(training, n.na) > n.row/2 )
training <- training[, -na.id]
dim(training)


##### Spliting the training data to training subset and testing subset #####
inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
training.in <- training[inTrain,];  testing.in <- training[-inTrain,]
dim(training.in);  dim(testing.in)

##### Tree model #####
modFit.rpart <- train(classe ~ ., method="rpart", data=training.in)
print(modFit.rpart)
fancyRpartPlot(modFit.rpart$finalModel)

pr.train.rpart <- predict(modFit.rpart, newdata=training.in)   # Training subset
confusionMatrix(pr.train.rpart, training.in$classe)

pr.test.rpart <- predict(modFit.rpart, newdata=testing.in)     # Testing subset
confusionMatrix(pr.test.rpart, testing.in$classe)


##### Random forest #####
modFit.rf <- train(classe ~ ., method="rf", data=training.in, prox=TRUE)
# Error 로 수행 불가
# print(modFit.rf)
#getTree(modFit.rf$finalModel, k=5)
#
#pr.train.rf <- predict(modFit.rf, newdata=training.in)
#confusionMatrix(training.in$classe, pr.train.rf)

#pr.test.rf <- predict(modFit.rf, newdata=testing.in)
#confusionMatrix(testing.in$classe, pr.test.rf)

##### lda method #####
modFit.lda <- train(classe ~ ., method="lda", data=training.in)
# Error 로 불가
# print(modFit.lda)
#pr.train.lda <- predict(modFit.lda, newdata=training.in)
#confusionMatrix(training.in$classe, pr.train.lda)
#pr.test.lda <- predict(modFit.lda, newdata=testing.in)
#confusionMatrix(testing.in$classe, pr.test.lda)

##### nb method #####
modFit.nb <- train(classe ~ ., method="nb", data=training.in)
# Error 로 불가
#print(modFit.nb)
#pr.train.nb <- predict(modFit.nb, newdata=training.in)
#confusionMatrix(training.in$classe, pr.train.nb)
#pr.test.nb <- predict(modFit.nb, newdata=testing.in)
#confusionMatrix(testing.in$classe, pr.test.nb)

##### Apply the tree model to pml.testing data #####
testing <- pml.testing[, c(8:160)]
factor.id <- which(sapply(testing, is.factor));  factor.n <- length(factor.id)

for(i in factor.id[-factor.n]){
  testing[, i] <- as.character(testing[, i])
  testing[, i] <- as.numeric(testing[, i])
}

testing[is.na(testing)] <- 0
predict(modFit.rpart, newdata=testing)



##### Unsupervised learning #####
kMeans1 <- kmeans(subset(training.in, select=-c(classe)), centers=5)

res.kmeans <- kMeans1$cluster
res.kmeans[res.kmeans==1] <- "A";  res.kmeans[res.kmeans==2] <- "B";  
res.kmeans[res.kmeans==3] <- "C";  res.kmeans[res.kmeans==4] <- "D";  
res.kmeans[res.kmeans==5] <- "E"
res.kmeans <- as.factor(res.kmeans)
training.in$clusters <- res.kmeans

confusionMatrix(training.in$clusters, training.in$classe)

modFit.kmeans <- train(clusters ~ ., data=subset(training.in, select=-c(classe)), method="rpart")
pr.train.kmeans <- predict(modFit.kmeans, newdata=training.in)
confusionMatrix(pr.train.kmeans, training.in$classe)

pr.test.kmeans <- predict(modFit.kmeans, newdata=testing.in)
confusionMatrix(pr.test.kmeans, testing.in$classe)

predict(modFit.kmeans, newdata=testing)


##### preProcessing with PCA #####
preProc <- preProcess( subset(training.in, select=-c(classe)), method="pca", center=5 )
#zero variance : 
#kurtosis_yaw_belt, 1
#skewness_yaw_belt,  1
#amplitude_yaw_belt, 
#kurtosis_yaw_dumbbell,  1
#skewness_yaw_dumbbell,  1
#amplitude_yaw_dumbbell, 
#kurtosis_yaw_forearm,  1
#skewness_yaw_forearm,  1
#amplitude_yaw_forearm


##### imputation - knnImpute #####
preObj <- preProcess(tr.red[, -factor.n], method="knnImpute")
res <- predict(preObj, tr.red[, -factor.n])

# names(pml.training)[c(1:7,160)]
# c(1:7, 160)
#[1] "X"                    "user_name"            "raw_timestamp_part_1" "raw_timestamp_part_2"
#[5] "cvtd_timestamp"       "new_window"           "num_window"           "classe" 