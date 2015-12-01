install.packages("Rcpp")
install.packages("microbenchmark")
library(Rcpp)
library(microbenchmark)

#Baby steps--------------------------------------------------------------------
Rfunc <- cppFunction('double ourFunc()
                     {
                     return runif(1)[0];
                     }')

Rfunc()
#What results do you get?
#0.60
Rfunc
#function () 
#.Primitive(".Call")(<pointer: 0x0000000014be1b80>)
write('#include <Rcpp.h>
  
  using namespace Rcpp;

// [[Rcpp::export]]
double ourFunc()
{
return runif(1)[0];
}',file='lab3_1.cpp')
sourceCpp('lab3_1.cpp')
ourFunc()
#0.3453303, yes it works

# Indefinite sums ---------------------------------------------------------

write('#include <Rcpp.h>

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
      }',file='lab3_2.cpp')
sourceCpp('lab3_2.cpp')
#When does increasing k stop improving the error?
logapprox(0.5, 20)-log(0.5)    # min error  -1.211139e-07 at k=20
logapprox(0.01,846)-log(0.01)  # min error 1.710219e-05 at k=846
#Is the same value of k applicable? No

#double
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
      }", file="lab3_2_1.cpp")
sourceCpp("lab3_2_1.cpp")
#Does the error decrease for 0.5 and 0.01 with the same k as before?
logapprox(0.5, 20) - log(0.5)  # min error 4.350892e-08 at k=20 same K
logapprox(0.01,846)-log(0.01)# min error 2.143963e-05 at same k

#What happens now if you increase k?
logapprox(0.5, 20) - log(0.5) # min error 2.220446e-16 at k=48
logapprox(0.01, 3000) - log(0.01) #  min error of 3.197442e-14 at k=3000
#Which ones of these do you think could be most relevant/easy to try here?
#summing terms in decreasing order

# Kahan sum ---------------------------------------------------------------
write('#include <Rcpp.h>
      
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
  }',file='lab3_3_1.cpp')

sourceCpp('lab3_3_1.cpp')
#Are you able to reduce the error for single 
#and double precision to the expected range for x = 0.01 
#and a large enough k, using Kahan's algorithm?
logapprox(0.5, 21) - log(0.5)      # min error -1.904654e-09 at k=21
logapprox(0.01, 1500) - log(0.01)  # min error of  -6.395087e-08 at k=1500 
#double
write('#include <Rcpp.h>
      
      using namespace Rcpp;
      
      // [[Rcpp::export]]
      double logapprox(double x, int k)
      {
      x = 1.0 - x;
      double c = 0;
      double sum = 0;	
      for (int i = 1; i <= k; i++)
      {
      
      double term = -1.0 / i * pow(x, i) ;
      double  y = term - c;
      term = sum + y;
      c = (term - sum) - y;
      sum = term;
      }
      
      return sum;
      }',file='lab3_3_2.cpp')

sourceCpp('lab3_3_2.cpp')
logapprox(0.5, 50)-log(0.5)       #min error 0  Perfect
logapprox(0.01, 10000)-log(0.01)  #min error 8.881784e-16


# Timming results ---------------------------------------------------------

logapproxR1 <- function( x,  k) {  # R loop
  x = 1.0 - x
  sum = 0
  for (i in seq(k))
  {
    term = -x**i
    sum  = sum + 1.0 / i * term
  }
  return (sum)
}

logapproxR2 <- function( x,  k) {  # R vector
  x = 1.0 - (x)
  vec = rep(x,k)
  pow = -1.0 / seq(k) * vec**seq(k)
  sum  = sum(pow)
  return (sum)
}
library(microbenchmark)
k<- 10
microbenchmark(logapprox(0.01,k), logapproxR1(0.01, k), logapproxR2(0.01, k))
# Unit: microseconds
# expr    min      lq     mean  median      uq    max neval
# logapprox(0.01, k)  1.418  2.3025  3.19889  2.8340  3.1890 14.169   100
# logapproxR1(0.01, k) 18.421 19.4830 23.21658 20.0145 21.6085 76.160   100
# logapproxR2(0.01, k) 17.003 17.7120 19.90797 18.2430 18.9520 42.508   100

# Compiler ----------------------------------------------------------------
library(compiler)
logapproxRcompiled <- cmpfun(logapproxR1)
microbenchmark(logapprox(0.01,k), logapproxR1(0.01, k), logapproxRcompiled(0.01,k), logapproxR2(0.01, k))
# Unit: microseconds
# expr    min     lq     mean  median      uq    max neval
# logapprox(0.01, k)  1.417  2.834  3.35836  3.1880  3.5430 14.170   100
# logapproxR1(0.01, k) 18.066 19.837 23.51767 20.1910 24.7965 76.514   100
# logapproxRcompiled(0.01, k)  8.501  9.210 12.25299  9.5645 10.6270 60.220   100
# logapproxR2(0.01, k) 17.003 17.712 19.87264 18.0660 18.9520 57.740   100


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
      }",file="lab3_4.cpp")
sourceCpp("lab3_4.cpp")
microbenchmark(logapprox(0.01,k), logapprox2(0.01,k),logapproxR1(0.01, k), logapproxR2(0.01,k), logapproxRcompiled(0.01, k))
# Unit: microseconds
# expr    min     lq     mean median      uq    max neval
# logapprox(0.01, k)  1.771  2.480  3.23081  3.188  3.5430  7.439   100
# logapprox2(0.01, k)  1.063  2.126  3.17419  2.480  3.5420 40.383   100
# logapproxR1(0.01, k) 19.129 20.900 24.82472 21.609 26.2130 53.135   100
# logapproxR2(0.01, k) 18.066 19.129 22.35208 19.837 21.0770 52.426   100
# logapproxRcompiled(0.01, k)  9.210  9.565 12.41254 10.273 10.6275 64.470   100


