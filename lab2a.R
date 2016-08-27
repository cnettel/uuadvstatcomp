#####################################################
### Advanced Statistical Computing Course 
### Lab 2
### Winter 2015
### Report by Anne-Gaelle Dosne
#####################################################

### Optimization

myfun <- function(x) {               # create a univariate function
  (x - 3)**2 + 2 * (x - 3)**2 + 3 * (x - 15)**2 + sin(100*x)
}

myfun(3)                                     # test the function
opt1 <- optimize(f=myfun, interval=c(0,100)) # optimize it over the interval from 0 to 100 (local minimization): min: 9.03
myfun(opt1$minimum)                          # returns function value at minimum (~215)
optimize(f=myfun, interval=c(0,15))          # min: 9.28
optimize(f=myfun, interval=c(9,12))          # min: 9.53
optimize(f=myfun, interval=c(10,11))         # min: 10.16
optimize(f=myfun, interval=c(-1000,1000))    # min: 8.71
# The optimum changes a little every time but is close to 9 whenever 9 is in the interval. 
# The value of the minimum varies by up to 0.5.
# There is no guarantee that this optimum is the global optimum, but it is the best over -1000,1000.

### Integrate a function

multsin <- function(x) {x*sin(x)} # create a new function
integrate(multsin,lower=-7E5,upper=7E5,subdivisions=1E7) # result: 1356376
system.time(integrate(multsin,lower=-7E5,upper=7E5,subdivisions=1E7)) # integrate it over large range and small steps
# takes ~12 seconds

library(parallel) # make it parallel
p_multsin <- function(lim) { # create function that integrate multsin in parallel
  y <- integrate(function(x) {x*sin(x)}, 
                 lower = lim[1], 
                 upper = lim[2], 
                 subdivisions = 1E7)
  return(y$value)
}
p_multsin(c(-7E5,7E5)) # test function: same result as before so OK

p_integrate <- function(nodes) {
  cl        <- makePSOCKcluster(nodes)
  intervals <- seq(from = -7E5, to = 7E5, length.out = nodes + 1) # creates nodes intervals
  min       <- intervals[-length(intervals)] # create vector of min values to use as lower arguments to integrate function
  max       <- intervals[-1]                 # create vector of max values to use as upper arguments to integrate function
  
  out <- parLapply(cl = cl, X = Map(c,min,max), fun = p_multsin)
  return(sum(unlist(out)))
}

p_integrate(2) # result: 1356376, same as before so function OK
p_integrate(4) # same
system.time({p_integrate(2)})  # takes 4 sec
system.time({p_integrate(4)})  # takes 2.9 sec --> best
system.time({p_integrate(8)})  # takes 3 sec
# Speed up 4-fold using parallel computing on 4 nodes (from 12 to 3 seconds)

### Functional operators: memoisation

require(memoise)

fib <- function(n) {  # no memoisation
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}

fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})

fib3 <- memoise(fib)

system.time(fib(28))   # takes 1.6 sec
system.time(fib2(28))  # takes 0.01 sec --> fastest
system.time(fib3(28))  # takes 1.6 sec
# forget(fib3)

### Domain specific languages: ggplot

library(ggplot2)

# Lab example
str(mpg) # dataframe, 234 obs and 11 variables
qplot(displ, hwy , data=mpg)                      # relation between engine displacement and highway miles per gallon
qplot(displ, hwy , data=mpg, color =drv)          # color legend: drive
qplot(displ, hwy, data =mpg, geom=c("point","smooth")) # add global statistic: smooth
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm") # add global statistic: linear model
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm",color=drv) # add local statistic by drive type
gr <- qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm",color=drv)
gr + theme(panel.background = element_rect(fill = "pink")) # change background color

# Exercise
str(diamonds) # dataframe, 53940 obs and 10 variables
qplot(carat, price , data=diamonds)               # relation between carat and price (expected to increase): looks exponential
qplot(carat, price , data=diamonds, color=color)  # color legend: color --> increase steeper in alphabetical color order
qplot(carat, price , data=diamonds, geom=c("point","smooth"),color=color) # add local statistic by color type: difficult to see
qplot(carat, price , data=diamonds, geom=c("smooth"),color=color) # remove points to see potential differences between smooths
qplot(carat, price , data=diamonds, geom=c("point","smooth")) # add global statistic

gr <- qplot(carat, price , data=diamonds, color=color)
gr + scale_color_discrete(name='Diamond color') # change title of color legend

qplot(color, price/carat , data=diamonds, geom="boxplot") # draw boxplot of price/carat for the different colors

# END
###########################################################################

