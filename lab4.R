setwd("/Users/Sebastian/Dropbox/Skola/Doktorand/Advanced statistical computing")
library(Rcpp)
# First function
Rfunc <- cppFunction('double ourFunc()
                     {
                     return runif(1)[0];
                     }')
Rfunc()

# Source the function
sourceCpp("lab4.cpp")
ourFunc()

# Load using Rcpp
sourceCpp("lab4_2.cpp") # logapprox, the function given in the lab
logapprox(0.5, 10)

# After k = 846, everything is the same
# Find this using while()
ind <- 0
k <- 1
comparevalue <- logapprox(0.01, 100000000)
while (ind == 0) {
  const <- logapprox(0.01, k)
  if ((const - comparevalue) == 0) {
    break
  } else {
    k <- k + 1
  }
}
k
const <- logapprox(0.01, 846) 
logapprox(0.01, 100000) - const
logapprox(0.01, 1000000) - const
logapprox(0.01, 10000000) - const

# The best error is within e-05, not e-07
as.character(const-log(0.01))
diff <- difference(0.01)

sourceCpp("lab4_3.cpp") # logapprox2, the function given in the lab but with double
# After k = 2730, everything is the same
ind <- 0
k <- 1000
comparevalue <- logapprox2(0.01, 100000000)
while (ind == 0) {
  const <- logapprox2(0.01, k)
  if ((const - comparevalue) == 0) {
    break
  } else {
    k <- k + 1
  }
}
k
const <- logapprox2(0.01, 2730)
logapprox2(0.01, 100000) - const
logapprox2(0.01, 1000000) - const
logapprox2(0.01, 10000000) - const

# The best error is within e-14, not e-16
as.character(const-log(0.01))

sourceCpp("lab4_4.cpp") # logapprox3, backwards summation with float
# After k = 1203, everything is the same
ind <- 0
k <- 1000
comparevalue <- logapprox3(0.01, 100000000)
while (ind == 0) {
  const <- logapprox3(0.01, k)
  if ((const - comparevalue) == 0) {
    break
  } else {
    k <- k + 1
  }
}
k
const <- logapprox3(0.01, 1203)
logapprox3(0.01, 100000) - const
logapprox3(0.01, 1000000) - const
logapprox3(0.01, 10000000) - const

# Error is within e-08
as.character(const-log(0.01))

sourceCpp("lab4_5.cpp") # logapprox4, backwards summation with double
# After k = 3452, everything is the same
ind <- 0
k <- 1000
comparevalue <- logapprox4(0.01, 100000000)
while (ind == 0) {
  const <- logapprox4(0.01, k)
  if ((const - comparevalue) == 0) {
    break
  } else {
    k <- k + 1
  }
}
k
const <- logapprox4(0.01, 3452)
logapprox4(0.01, 100000) - const
logapprox4(0.01, 1000000) - const
logapprox4(0.01, 10000000) - const

# Error is exactly 0!!! or...?
as.character(const-log(0.01))

sourceCpp("lab4_6.cpp") # logapprox5, backwards summation with Kahan's and double
# After k = 3101, everything is the same
ind <- 0
k <- 1000
comparevalue <- logapprox5(0.01, 100000000)
while (ind == 0) {
  const <- logapprox5(0.01, k)
  if ((const - comparevalue) == 0) {
    break
  } else {
    k <- k + 1
  }
}
k
const <- logapprox5(3452)
logapprox5(0.01, 100000) - const
logapprox5(0.01, 1000000) - const
logapprox5(0.01, 10000000) - const

# Error is within e-16
as.character(const-log(0.01))

# Using loops
logapproxR <- function(x, k) {
  x <- 1 - x
  summation <- 0
  for (i in 1:k) {
    summation <- summation -1/i*x^i
  }
  summation
}

# Compiled version
logapproxR2 <- cmpfun(logapproxR)

# Vectorized(?) version
logapproxR3 <- function(x, k) {
  x <- 1 - x
  -cumprod(rep(0.99, k)) %*% (1/(1:k))
}

# Compiled version
logapproxR4 <- cmpfun(logapproxR3)

# My vectorized function wins!
microbenchmark(logapproxR(0.01, 10000), logapproxR2(0.01, 10000), logapproxR3(0.01, 10000), logapproxR4(0.01, 10000),
               logapprox(0.01, 10000), logapprox4(0.01, 10000))

# But not anymore...
sourceCpp("lab4_8.cpp")
microbenchmark(logapproxR(0.01, 10000), logapproxR2(0.01, 10000), logapproxR3(0.01, 10000), logapproxR4(0.01, 10000),
               logapprox(0.01, 10000), logapprox4(0.01, 10000), logapprox7(0.01, 10000))
