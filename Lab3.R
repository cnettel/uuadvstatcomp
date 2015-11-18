#####################################################
### Advanced Statistical Computing Course 
### Lab 3
### Winter 2015
### Author: Anne-Gaelle Dosne
#####################################################

install.packages("Rcpp","microbenchmark")
library(Rcpp)
library(microbenchmark)
library(compiler)

### Baby steps for using C++ in R 

Rfunc <- cppFunction('double ourFunc()
                     {
                     return runif(1)[0];
                     }');

write("#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double ourFunc()
{
return runif(1)[0];
}", file="lab3_1.cpp")

sourceCpp("lab3_1.cpp")

### Indefinite sums: taylor expansion using C++ in R 

write("#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox(double x, int k)
{
x = 1.0 - x;

float sum = 0;	
for (int i = 1; i <= k; i++)
{
  float term = pow(x,i);
  sum -= 1.0 / i * term;
}

return sum;
}", file="lab3_2.cpp")
sourceCpp("lab3_2.cpp")

logapprox(0.5, 10) - log(0.5) # min error of 10E-7 at k=15
logapprox(0.01, 1000) - log(0.01) # min error of 10E-5 at k=1000

# Increase precision

write("#include <Rcpp.h>   
      
      using namespace Rcpp;
      
      // [[Rcpp::export]]
      double logapprox(double x, int k)
      {
      x = 1.0 - x;
      
      double sum = 0;	
      for (int i = 1; i <= k; i++)
      {
      double term = pow(x,i);
      sum -= 1.0 / i * term;
      }
      
      return sum;
      }", file="lab3_2a.cpp")
sourceCpp("lab3_2a.cpp")

logapprox(0.5, 50) - log(0.5) # min error of 10E-16 at k=50
logapprox(0.01, 3000) - log(0.0.1) #  min error of 10E-14 at k=3000
# at same k precision not changed much
# double instead of float increases precision by 10E-9 but increase k by 3-fold
# if we are approx. logs for int < 1 we should use reverse order 
# doing partial summations would work also

### Use Kahan's summation to improve speed 

write("#include <Rcpp.h>
      
      using namespace Rcpp;
      
      // [[Rcpp::export]]
double logapprox(double x, int k)
{
x = 1.0 - x;
float c = 0;
float sum = 0;	
for (int i = 1; i <= k; i++)
{
  
 float term = -1.0 / i * pow(x, i) ;
 float  y = term - c;
 term = sum + y;
 c = (term - sum) - y;
 sum = term;
}

return sum;
}", file="lab3_kahan.cpp")
sourceCpp("lab3_kahan.cpp")

logapprox(0.5, 50) - log(0.5) # min error of 10E-09 at k=50 (single precision)

### Running it in R (without C++)

Rlogaprox1 <- function( x,  k) {  # R loop
x = 1.0 - x
sum = 0
for (i in seq(k))
{
  term = -x**i
  sum  = sum + 1.0 / i * term
}
return (sum)
}

Rlogaprox2 <- function( x,  k) {  # R vector
  x = 1.0 - (x)
  vec = rep(x,k)
  pow = -1.0 / seq(k) * vec**seq(k)
  sum  = sum(pow)
  return (sum)
}

Rlogaprox1(0.5,10) - log(0.5)
Rlogaprox1(0.5,100) - log(0.5)
Rlogaprox2(0.5,10) - log(0.5)

k<- 10
microbenchmark(logapprox(0.01,k), Rlogaprox1(0.01, k), Rlogaprox2(0.01, k))

# Unit: microseconds
# expr    min      lq     mean  median     uq    max neval
# logapprox(0.01, k)  3.881  4.4105  5.10747  5.0535  5.309 13.814   100
# Rlogaprox1(0.01, k) 23.287 24.5450 26.20380 25.3850 26.309 63.138   100
# Rlogaprox2(0.01, k) 21.538 22.7810 23.93945 23.3815 24.337 44.650   100

logapproxRcompiled <- cmpfun(Rlogaprox1) # twice as fast, but still slower than C++
microbenchmark(logapprox(0.01,k), Rlogaprox1(0.01, k), logapproxRcompiled(0.01,k), Rlogaprox2(0.01, k))

# Unit: microseconds
# expr    min      lq     mean  median      uq    max
# logapprox(0.01, k)  3.662  4.8100  5.55996  5.2695  6.0370 15.277
# Rlogaprox1(0.01, k) 22.459 24.2400 26.03471 24.8835 26.0440 65.527
# logapproxRcompiled(0.01, k) 11.451 12.4240 13.24135 12.9915 13.7495 21.564
# Rlogaprox2(0.01, k) 21.577 23.1265 24.90362 23.8380 24.7825 48.736

### Improve C++ function by avoiding powers

write("#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox2(double x, int k)
{
x = 1.0 - x;

float sum = 0;
float term = 1;	
for (int i = 1; i <= k; i++)
{
  float term = term*x;
  sum -= 1.0 / i * term;
}

return sum;
}",file="lab3_improvedC.cpp")
sourceCpp("lab3_improvedC.cpp") 
microbenchmark(logapprox(0.01,k), logapprox2(0.01,k),Rlogaprox1(0.01, k), logapproxRcompiled(0.01,k), Rlogaprox2(0.01, k))
# Unit: microseconds
# expr    min      lq     mean  median      uq    max
# logapprox(0.01, k)  3.748  4.5825  5.77765  5.2130  5.9395 22.575
# logapprox2(0.01, k)  2.695  3.4825  4.87535  4.3085  5.2325 29.666
# Rlogaprox1(0.01, k) 23.428 25.3755 29.74842 26.7185 31.5585 63.730
# logapproxRcompiled(0.01, k) 11.602 13.1580 14.89611 13.9190 15.0165 29.813
# Rlogaprox2(0.01, k) 22.291 23.8400 26.23098 24.8925 26.2105 51.443
# It improves quite a bit!!


### End
#################################3
