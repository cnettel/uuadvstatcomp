f<-function(x){
  y<-sum((x - 3)^2 + 2 * (x - 3) ^ 2 + 3 * (x - 15) ^ 2 + sin(100*x)) # the sin creates a local min
 return(y)
}

f(3)

optimise(f,c(0,15))
optimise(f,c(0,100))
optimise(f,c(9,12))
optimise(f,interval = c(10,11))

ff<-function(x) {
  y<-x*sin(x)
  return (y)
}

ff(2)

integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7 )

system.time(integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7))


integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7 )

z<-c(-7e+05:-350000)
b<-c(-350000:0)
a<-c(0:350000)
c<-c(350000:7e+05)



library(parallel)
cl<-makePSOCKcluster(4)
chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
chunk(d,5)



f2<-function(lim){
    y<-integrate(function(x) {x*sin(x)},
   lower=lim[1],
   upper=lim[2],
 subdivisions=1E7)
 return(y$value)
}

d<-seq(-7e5,7e5,length.out = 5)
min<-d[-length(d)]
max<-d[-1]


system.time(parLapply(cl=cl,X = Map(c,min,max),fun = f2))


# memoise -----------------------------------------------------------------

library(memoise)
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

# library(ggplot2) --------------------------------------------------------

library(ggplot2)
str(mpg)
qplot(displ, hwy , data=mpg)
qplot(displ, hwy , data=mpg, color =drv)
qplot(displ,hwy,data=mpg,geom=c('point','smooth'),method='loess')
#You can check the linear relationship for different groups:
qplot(displ,hwy,data=mpg,color=drv,geom=c('point','smooth'),method='lm')


# As we mentioned, one benefit of these plots is that you can modify them after they have been created.
# For example, you can use theme() to modify theme setting. We assign the graph to variable:gr <-
# and then we can modify it. Let's change the color of the rectangluar elements to pink:

gr<-qplot(displ,hwy,data=mpg,color=drv,geom=c('point','smooth'),method='lm')
gr + theme(panel.background = element_rect(colour = "black"))
gr + theme(panel.background = element_rect(colour = "blue"))


#Load the data set diamonds from ggplot2.
str(diamonds)
qplot(carat,price,data=diamonds)
qplot(carat,price,data=diamonds,color=color)
qplot(carat,price,data=diamonds,,color=color,geom=c('point','smooth'),method='lm')
gr<-qplot(carat,price,data=diamonds,color=color,geom=c('point','smooth'),method='loess')
gr + theme(legend.text='New legends')
?theme()
#Find out how to draw a boxplot to check the distribution of price/carat for different colors
qplot(x=color,y=price/carat,data=diamonds,color=color,geom='boxplot')
?qplot()
