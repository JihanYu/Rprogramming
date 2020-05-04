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


a <- 0.05;  b <- 0.2
effect.size <- 5
mu0 <- 0;  std.drv0 <- 1
mu1 <- mu0 + effect.size;  std.drv1 <- 1
one.two <- TRUE
sample.size <- NULL
#std.err0 <- std.drv0 / sqrt(n0/2)
#std.err1 <- std.drv1 / sqrt(n1/2)

two.sided <- 1;  if(one.two == TRUE) two.sided <- 2
x.inter <- qnorm(a/two.sided, lower.tail=FALSE)

x <- seq(from = mu0 - 5*std.drv0, to = mu1 + 5*std.drv1, by=0.01)
h0 <- dnorm(x, mean=mu0, sd=std.drv0)
h1 <- dnorm(x, mean=mu1, sd=std.drv1)
h.norm <- data.frame(x, h0, h1)

p <- ggplot(h.norm, aes(x=x, y=h0)) +
	geom_line(aes(x=x, y=h0), color="red") +
	geom_line(aes(x=x, y=h1), color="blue") +
	geom_vline(xintercept=x.inter) +
	geom_ribbon(data=subset(h.norm, x.inter <= x & x <= max(x)), 
				aes(ymin=0, ymax=h0, fill="H0", alpha=0.5)) +
	geom_ribbon(data=subset(h.norm, min(x) <= x & x <= x.inter),
				aes(ymin=0, ymax=h1, fill="H1", alpha=0.5))
print(p)
