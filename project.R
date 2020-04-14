workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\DS08 Practical machine learning"
setwd(workingpath)

pml.training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header=TRUE)
pml.testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=TRUE)

# predict the manner of exercise : "classe"

library(caret);  library(ggplot2)

inTrain <- createDataPartition(pml.training$classe, p=0.7, list=FALSE)

training <- pml.training[inTrain,];  training <- training[, c(8:160)]
testing <- pml.training[-inTrain,];  testing <- testing[, c(8:160)]

dim(training);  dim(testing)

# names(pml.training)[c(1:7,160)]
# c(1:7, 160)
#[1] "X"                    "user_name"            "raw_timestamp_part_1" "raw_timestamp_part_2"
#[5] "cvtd_timestamp"       "new_window"           "num_window"           "classe" 

div.ch <- "#DIV/0!"

factor.id <- which( sapply(training, is.factor) )

a <- training[, factor.id];  b <- sapply(a, class)

nc <- ncol(a) -1
for(i in c(1:nc)){
	div.id <- which(a[,i]==div.ch)
	a[div.id,i] <- NA
}


kMeans1 <- kmeans(subset(training, select=-c(classe)), centers=3)

