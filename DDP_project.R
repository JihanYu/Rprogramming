workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\project"
setwd(workingpath)

library(ggplot2)

p <- ggplot(data.frame(x = c(-3, 3)), aes(x=x))
p + stat_function(fun=dnorm)

p + stat_function(fun=dt, args=list(df=2))

myfun <- function(xvar){
  1/(1+exp(-xvar+10))
}
ggplot(data.frame(x=c(0, 20)), aes(x=x)) + stat_function(fun=myfun)

p <- ggplot(data.frame(x = c(-3, 3)), aes(x=x))
f1.norm <- function(x, mu, std.dev){
	dnorm(x=x, mean=mu, sd=std.dev)
}

f2.norm <- function(x, mu, std.dev){
	dnorm(x=x, mean=mu, sd=std.dev)
}

p + stat_function(fun=f1.norm(mu=0, std.dev=1)) + stat_function(fun=f2.norm(mu=1, std.dev=2))

x <- seq(from=-2, to=2, by=0.01)
y1 <- dnorm(x)
y2 <- dt(x, df=1)
df.a <- data.frame(x, y1, y2)

ggplot(df.a, aes(x=x, y=y1)) +
	geom_line(aes(y=y1)) +
	geom_line(aes(y=y2)) +
	geom_ribbon(data=subset(df.a, 1 <= x & x <= 1.5),
				aes(ymin=0, ymax=y1), fill="blue", alpha=0.5) 


x <- seq(from=-5, to=5, by=0.01)
h0 <- dnorm(x, mean=mu0, sd=std.drv0)
h1 <- dnorm(x, mean=mu1, sd=std.drv1)
h.norm <- data.frame(x, h0, h1)

a <- 0.05;  b <- 0.2

p <- ggplot() +
	geom_line(df.a, aes(x=x, y=y1), color="blue") +
	geom_line(df.a, aes(x=x, y=y2), color="red") +
	geom_vline(xintercept=1)
