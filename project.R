workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\DS08 Practical machine learning"
setwd(workingpath)

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header=TRUE)
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=TRUE)

# predict the manner of exercise : "classe"
