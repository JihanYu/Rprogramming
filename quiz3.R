library(datasets)
data(iris)

mean(iris[iris$Species=="virginica",]$Sepal.Length, na.rm=TRUE)

apply(iris[, 1:4], 2, mean)

data(mtcars)

tapply(mtcars$cyl, mtcars$mpg, mean)
sapply(mtcars, cyl, mean)
with(mtcars, tapply(mpg, cyl, mean))  #O
sapply(split(mtcars$mpg, mtcars$cyl), mean)  #O
apply(mtcars, 2, mean)
split(mtcars, mtcars$cyl)
tapply(mtcars$mpg, mtcars$cyl, mean)  #O
mean(mtcars$mpg, mtcars$cyl)
lapply(mtcars, mean)

abs( mean(mtcars[mtcars$cyl==4,]$hp, na.rm=TRUE) - 
  mean(mtcars[mtcars$cyl==8,]$hp, na.rm=TRUE) )

debug(ls)
