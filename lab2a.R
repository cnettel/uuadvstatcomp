
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
