
# Part 1 : Optimization ---------------------------------------------------

## Create function
f <- function(x){
  y <- sum((x - 3)^2 + 2 * (x - 3)^2 + 3 * (x - 15)^2 + sin(100*x))
  return(y)
}

## Test on single value
f(1)

## Optimize f on wide range
optimise(f, c(0,100))

# Optimize f on small ranges
optimise(f, c(0,15))
optimise(f, c(9,12))
optimise(f, c(10,11))

# Part 2 : Integrating a function -----------------------------------------

## Create function
f2 <- function(x) {
  y <- x*sin(x)
  return(y)
}

## Standard execution
system.time({
integrate(f2, 
          lower = -7E5, 
          upper = 7E5, 
          subdivisions = 1E7)
}) # 9.288s

## Create batch function
f3 <- function(lim) {
  y <- integrate(function(x) { x*sin(x) }, 
                 lower = lim[1], 
                 upper = lim[2], 
                 subdivisions = 1E7)
  return(y$value)
}

## Parallel execution
library(parallel)
nodes     <- 4

p_integrate <- function(nodes) {
  cl        <- makePSOCKcluster(nodes)
  intervals <- seq(from = -7E5, to = 7E5, length.out = nodes + 1)
  min       <- intervals[-length(intervals)]
  max       <- intervals[-1]
  
  out <- parLapply(cl = cl, X = Map(c,min,max), fun = f3)
  
  return(sum(unlist(out)))
}

system.time({p_integrate(2)})  # 2.810s 
system.time({p_integrate(4)})  # 2.253s Best
system.time({p_integrate(8)})  # 2.606s
system.time({p_integrate(16)}) # 3.537s


# Memoisation -------------------------------------------------------------
## Define functions
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

## Benchmark
system.time(fib(28))  # 0.753s  # Retry: 0.722s
system.time(fib2(28)) # 0.005s  # Retry: 0.001s
system.time(fib3(28)) # 0.728s  # Retry: 0.728s

forget(fib3)



# ggplot ------------------------------------------------------------------

library(ggplot2)

## Examples
str(mpg)
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, color = drv)
qplot(displ, hwy, data = mpg, geom = c("point", "smooth")) 
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method = "lm") 
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method = "lm", color = drv)

gr <- qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method = "lm", color = drv)
gr + theme(panel.background = element_rect(fill = "pink"))

## Exercice
# 1. Load the data set diamonds from ggplot2.
head(diamonds)

# 2. Look at the variables carat and price in the dataframe and plot them against each other.
qplot(carat, price, data = diamonds, geom = 'point')

# 3. Look at the relation between carat and price for different diamond colors.
qplot(carat, price, data = diamonds, geom = 'point', color = color)

# 4. Add a smoother to the plot.
qplot(carat, price, data = diamonds, geom = c('point','smooth'), color = color, method = 'loess')

# 5. Change the title of legend of your plot to “New legends”. 
# Tips: look at the theme() help to find out how to change the title of legend.
qplot(carat, price, data = diamonds, geom = c('point','smooth'), color = color, method = 'loess')+
  scale_color_discrete(name='Diamond\ncolors:')

# 6. Find out how to draw a boxplot to check the distribution of price/carat for different colors.
qplot(color, price/carat, data = diamonds, geom = 'boxplot', fill = color)

# Add and commit a representative part of your plot work to your github project.
# For reference, here are very useful videos to learn more about ggplot2:
# Plotting with ggplot2: Part 1: https://www.youtube.com/watch?v=HeqHMM4ziXA
# Plotting with ggplot2: Part 2: https://www.youtube.com/watch?v=n8kYa9vu1l8