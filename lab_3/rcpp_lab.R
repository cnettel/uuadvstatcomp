# Baby steps --------------------------------------------------------------

library(Rcpp)

# 1. What do you write? 
## You call the function like a regular R function
Rfunc <- cppFunction('double ourFunc()
{
  return runif(1)[0];
  }')
Rfunc()

# 2. What results do you get?
## We get a sample from the uniform distribution 0.235143

# 3. What's the contents of this function?
## function () 
##  .Primitive(".Call")(<pointer: 0x105febe30>)

# sourceCpp
# 4. Does it work?
## Yes
sourceCpp('lab3_1.cpp')
ourFunc()
## 0.3884162


# Indefinite Sums ---------------------------------------------------------

sourceCpp('lab3_2.cpp')

# 1. When does increasing k stop improving the error?
logapprox(0.5, 10) - log(0.5) # 8.225251e-05
logapprox(0.5, 15) - log(0.5) # 1.72663e-06
logapprox(0.5, 20) - log(0.5) # -1.211139e-07 # k = 20
logapprox(0.5, 25) - log(0.5) # -1.211139e-07

# 2. Is the same value of k applicable? 
logapprox(0.01, 20) - log(0.01) # 1.198299
logapprox(0.01, 800) - log(0.01) # 3.90367e-05
logapprox(0.01, 845) - log(0.01) # 1.757902e-05
logapprox(0.01, 846) - log(0.01) # 1.710219e-05 # k = 846
logapprox(0.01, 847) - log(0.01) # 1.710219e-05

# 3. How big is the best error you can achieve?
## 1.710219e-05


# 4. Does the error decrease for 0.5 and 0.01 with the same k as before? 
sourceCpp('lab3_3.cpp')

## Yes is decreases (slightly) for 0.5
logapproxd(0.5, 20) - log(0.5) # 4.350892e-08

## No it increases (slightly) for 0.01
logapproxd(0.01, 846) - log(0.01) # 2.143963e-05

# 5. What happens now if you increase k?
## We get much smaller error with 0.5 close to 1e-16
logapproxd(0.5, 47) - log(0.5) # 3.330669e-16
logapproxd(0.5, 48) - log(0.5) # 2.220446e-16 # k = 48
logapproxd(0.5, 49) - log(0.5) # 2.220446e-16

## We get much smaller error with 0.01 but not yet at 1e-16
logapproxd(0.01, 2729) - log(0.01) # 3.28626e-14
logapproxd(0.01, 2730) - log(0.01) # 3.197442e-14 # k = 2730
logapproxd(0.01, 2731) - log(0.01) # 3.197442e-14

# 6. Which ones of these do you think could be most relevant/easy to try here?
##    Summing terms in decreasing order would probably help as the values 
##    get smaller and samller

# 7. Are you able to reduce the error for single and double precision to the 
#    expected range for x = 0.01 and a large enough k, using Kahan's algorithm or 
#    some other, simpler approach?

## 0.5 single precision = Better
sourceCpp('lab3_4.cpp')
logapproxk(0.5, 48) - log(0.5) # -1.904654e-09

## 0.01 single precision = Better
logapproxk(0.01, 2730) - log(0.01) # -6.395087e-08

## 0.5 double precision = Better
sourceCpp('lab3_5.cpp')
logapproxkd(0.5, 48) - log(0.5) # 0

## 0.01 single precision = Better
logapproxkd(0.01, 4000) - log(0.01) # 8.881784e-16


# Timing results ----------------------------------------------------------
## R loop
logapproxR1 <- function(x, k) {
  x   = 1.0 - x
  sum = 0
  for (i in 1:k){
    term = x^i
    sum = sum - 1.0 / i * term
  }
  return(sum)
}

## Vectorized form
logapproxR2 <- function(x, k) {
  x   = 1.0 - x
  y = - 1.0 / (1:k) * x^(1:k)
  sum = sum(y)
  
  return(sum)
}

## Benchmark
library(microbenchmark)
microbenchmark( 
  logapproxd(0.01, k),
  logapproxR1(0.01, k),
  logapproxR2(0.01, k)
)

## Unit: microseconds
## expr                 min     lq        mean      median    uq        max       neval
## logapproxd(0.01, k)  4.868   5.2870    6.03893   5.7325    5.9595    15.321    100
## logapproxR1(0.01, k) 70.642  74.5285   83.63903  78.3055   82.6330   329.390   100
## logapproxR2(0.01, k) 6.489   7.0540    8.52782   7.5690    8.2225    31.586    100


# 1. How does the Rcpp version fare compared to an R loop and a vectorised R code?
# Rcpp is much faster than the R loop but the difference is much less significant 
# with the vectorized R code.

# Compiled versions
library(compiler)
logapproxR1compiled <- cmpfun(logapproxR1)
logapproxR2compiled <- cmpfun(logapproxR2)

microbenchmark( 
  logapproxd(0.01, k),
  logapproxR1compiled(0.01, k),
  logapproxR2compiled(0.01, k)
)

## Unit: microseconds
## expr                          min     lq        mean      median    uq        max      neval
## logapproxd(0.01, k)           5.342   5.8065    6.37469   6.0895    6.6165    21.145   100
## logapproxR1compiled(0.01, k)  18.147  18.5325   21.93833  19.6035   21.2105   64.729   100
## logapproxR2compiled(0.01, k)  7.033   7.3605    8.50502   8.0090    8.5820    35.666   100

## The compilation helped the function R1 but not R2. R1 is now faster but 
## still much slower than the vector form or the Rcpp.

# 2. Do you see any way to remove the evaluation of a pow and/or a division from the C++ code? 
#    Time your new version with microbenchmark on the x = 0.01 case and assess the benefit.
sourceCpp('lab3_6.cpp')
microbenchmark( 
  logapprox(0.01, k),
  logapproxf(0.01, k)
)

## Unit: microseconds
## expr                min   lq   mean     median  uq      max     neval
## logapprox(0.01, k)  6.251 6.36 6.64973  6.454   6.5675  21.797  100
## logapproxf(0.01, k) 2.589 2.69 2.85299  2.796   2.9025  6.836   100

## The function without the power function is >2x faster than the previous Rcpp version.
