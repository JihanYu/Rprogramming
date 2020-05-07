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


library(ggplot2)

a <- 0.05;  b <- 0.2
effect.size <- 0.5
mu0 <- 0;  std.drv0 <- 1;  std.drv1 <- 1
one.two <- TRUE
n1 <- NULL;  n1.n0 <- 1;  
two.sided <- 1;  

mu1 <- mu0 + effect.size;  
if(one.two == TRUE) two.sided <- 2
za <- qnorm(a/two.sided, lower.tail=FALSE)
zb <- qnorm(b, lower.tail=FALSE)

q12 <- NULL
if(n1.n0 >= 1){ 
	q12 <- 1 + 1/n1.n0
} else {
	q12 <- 1 + n1.n0
}

est.sample.size <- ( (za + zb)^2 * std.drv0^2 * (q12) ) / (effect.size^2)

if(is.na(n1)){
	n1 <- est.sample.size
}
n0 <- n1/n1.n0

std.err0 <- std.drv0 / sqrt(n0/2)
std.err1 <- std.drv1 / sqrt(n1/2)

x <- seq(from = mu0 - 5*std.err0, to = mu1 + 5*std.err1, by=0.01)
h0 <- dnorm(x, mean=mu0, sd=std.err0)
h1 <- dnorm(x, mean=mu1, sd=std.err1)
h.norm <- data.frame(x, h0, h1)

a.h0 <- qnorm(a/two.sided, mean=mu0, sd=std.err0, lower.tail=FALSE)
b.h1 <- qnorm(b, mean=mu1, sd=std.err1)

p <- ggplot(h.norm, aes(x=x, y=h0)) +
	geom_line(aes(x=x, y=h0), color="red") +
	geom_line(aes(x=x, y=h1), color="blue") +
	geom_vline(xintercept=a.h0) +
	geom_vline(xintercept=b.h1, linetype="dashed", show.legend=TRUE) +
	geom_ribbon(data=subset(h.norm, a.h0 <= x & x <= max(x)), 
				aes(ymin=0, ymax=h0, fill="H0", alpha=0.5)) +
	geom_ribbon(data=subset(h.norm, min(x) <= x & x <= a.h0),
				aes(ymin=0, ymax=h1, fill="H1", alpha=0.5))
print(p)

power.b <- pnorm(qnorm(a/two.sided, mean=mu0, sd=std.err0, lower.tail=FALSE), 
				 mean=mu1, sd=std.err1, lower.tail=FALSE)

cat("suggested sample size :", est.sample.size, "\n")

if (power.b >= (1-b)){
	cat("Power :", power.b, "- adequate\n")
} else{
	cat("Power :", power.b, "- inadequate\n")
}
