#OPTIMISE

y <- function(x) {(x-3)^2+2*(x-3)^2+3*(x-15)^2+sin(100*x)}

optimise(y, c(0,100))
optimise(y, c(0,15))
optimise(y, c(9,12))
optimise(y, c(10,11))
plot(y,0,30)

# we do not get the same optimum each time
# we get local optimums (not global optimum)

#INTEGRATE

function(x) {x*sin(x)}
integrate(function(x) {x*sin(x)}, lower = -7e5, upper = 7e5, subdivisions=1e7)
system.time(integrate(function(x) {x*sin(x)}, lower = -7e5, upper = 7e5, subdivisions=1e7))
# 18.14

#PARALLEL

#make subintervals of the given interval (-7e5,7e5)

library(parallel)
cl <- makePSOCKcluster(16)  #1.56 with cluster size 16
system.time(parLapply(cl,(-7:7)*1e5,(function(x) { integrate(function (x) { x*sin(x) }, x, x+1e5, subdivisions = 1e7)})))
stopCluster(cl)

#MEMOISE

fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}

fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
fib3 <- memoise(fib)

fib(28)
fib2(28)
fib3(28)
system.time(fib(28))
system.time(fib2(28))
system.time(fib3(28))  #fib3 is the fastest

#GGPLOT

#Example to use qplot

#first install ggplot2 package, then load the library:
#install.packages("ggplot2")  
library(ggplot2)
#We work with the data set mpg in ggplot2:
str(mpg)
#We want to look at the relation between engine displacement(displ) and highway miles per gallon(hwy):
qplot(displ, hwy , data=mpg)
#the data set has three different factors: drv. f = front-wheel drive, r = rear wheel drive, 4 = 4wd.
#We can plot the relation b/w displ and hwy for different factors using color=drv:
qplot(displ, hwy , data=mpg, color =drv)
#we can add statistics to the plot using geom. Let's add a smoother to a plot:
qplot(displ, hwy, data =mpg, geom=c("point","smooth"))
#we can also check the linear relationship for the data:
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm")
#we can check the linear relationship for different groups:
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm",color=drv)
#we can use theme() to modify theme setting.
#We first assign the graph to variable gr:
gr <- qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm",color=drv)
#then we can modify it,e.g change the color of the rectangular elements to pink:
gr + theme(panel.background = element_rect(fill = "pink"))

# Exercise

library(ggplot2)
str(diamonds)
qplot(carat, price , data=diamonds)
qplot(carat, price , data=diamonds, color =color)
qplot(carat, price , data=diamonds,geom=c("point","smooth"))
gr2 <- qplot(carat, price , data=diamonds,geom=c("point","smooth"))
gr2 + scale_colour_brewer(name = "New legends")
#boxplot(price~carat,data=diamonds,col=(c("gold","green")), main="boxplot", xlab="carat", ylab="price")
qplot(color, price/carat, data = diamonds, geom = 'boxplot', fill = color)
