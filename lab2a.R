rm(list=ls())

### 1. Optimization ###

SumFun <- function(x) {
  (x - 3)^2 + 2*(x - 3)^2 + 3*(x - 15)^2 + sin(100*x)
}      

optimise(SumFun, c(0,100))

optimise(SumFun, c(0,15))

optimise(SumFun, c(9,12))

optimise(SumFun, c(10,11))

### 2. Adding file lab2a.R in the repository C:\ uuadvstatcomp ###

### 3. Integrating a function ###

Fun1 <- function(x) {
  x*sin(x)
}

Integral <- integrate(Fun1, lower = -7e5, upper = 7e5,
          subdivisions = 1e7)

Integral

system.time (integrate(Fun1, lower = -7e5, upper = 7e5,
                       subdivisions = 1e7))


### 9. Getting a SUPR account (Done! xzh@du.se) ###