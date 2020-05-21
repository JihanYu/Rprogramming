workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\project\\Rprogramming"
setwd(workingpath)

##### Problem 1 #####
set.seed(1)
rpois(5,2)

# [1] 1 1 2 4 1

##### Problem 2 #####
# rnorm

##### Problem 3 #####
# It can be used to specify which random number generating algorithm R should use, ensuring consistency and reproducibility


##### Problem 4 #####
# qpois

##### Problem 5 #####
set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
# Generate data from a Normal linear model

##### Problem 6 #####
# rbinom

##### Problem 7 ##### 
# the function call stack

##### Problem 8 #####  -> XXXXX
library(datasets)
x1 <- 1:5;  x2 <- 6:10
y <- 11:15
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

summaryRprof()$by.total[,1][2] / sum(summaryRprof()$by.total[,1])

# 0.2142857 -> XXXXX
# 0.5

##### Problem 9 ##### 
# It is the time spent by the CPU evaluating an expression

##### Problem 10 ##### -> XXXXX
# user time is always smaller than elapsed time -> XXXXX
# elapsed time may be smaller than user time
