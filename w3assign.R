setwd("C:\\Users\\MED1\\Desktop\\Coursera\\project")
library(plotly); 

data(diamonds)

plot_ly(diamonds, x = ~price)
plot_ly(diamonds, x = ~price, y = ~carat)
plot_ly(diamonds, x = ~price, y = ~carat, color= ~cut)
plot_ly(diamonds, x = ~price, y = ~carat, color= ~color)
plot_ly(diamonds, x = ~price, y = ~carat, color= ~clarity)
plot_ly(diamonds, x = ~price, y = ~carat, z = ~cut, color = ~cut)
plot_ly(diamonds, x = ~price, y = ~carat, z = ~color, color = ~color)
plot_ly(diamonds, x = ~price, y = ~carat, z = ~clarity, color = ~clarity)





data(mpg)

mpg$manufacturer <- as.factor(mpg$manufacturer)
mpg$model <- as.factor(mpg$model)
mpg$year <- as.factor(mpg$year)
mpg$cyl <- as.factor(mpg$cyl)
mpg$trans <- as.factor(mpg$trans)
mpg$drv <- as.factor(mpg$drv)
mpg$fl <- as.factor(mpg$fl)
mpg$class <- as.factor(mpg$class)


plot_ly(mpg, x = ~cty)
plot_ly(mpg, x = ~cty, y = ~hwy)
plot_ly(mpg, x = ~cty, y = ~hwy, color = ~ cyl)
plot_ly(mpg, x = ~cty, y = ~hwy, color = ~ year)
plot_ly(mpg, x = ~cty, y = ~hwy, color = ~ drv)
plot_ly(mpg, x = ~cty, y = ~hwy, color = ~ fl)

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl, color = ~cyl)
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~year, color = ~year)
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~drv, color = ~drv)
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~fl, color = ~fl)


plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
	add_markers(color = ~cyl)

plot_ly(diamonds, x = ~x, y = ~y, z = ~z, color = ~z)

plot_ly(diamonds, x = ~price, y = ~carat, z = ~cut, color = ~cut)
plot_ly(diamonds, x = ~price, y = ~carat, z = ~color, color = ~color)
plot_ly(diamonds, x = ~price, y = ~carat, z = ~clarity, color = ~clarity)
plot_ly(diamonds, x = ~depth, y = ~table, z = ~cut, color = ~cut)
plot_ly(diamonds, x = ~depth, y = ~table, z = ~color, color = ~color)
plot_ly(diamonds, x = ~depth, y = ~table, z = ~clarity, color = ~clarity)
