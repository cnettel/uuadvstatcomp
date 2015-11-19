#################################################
#                                               #
# Lab3_Block 3 (Advanced statistical computing) #
#                                               #
#                                               #
#################################################
library (Rcpp)

## Baby Steps ##

double ourFunc()
{
return runif(1)[0];
} 
# This function does not run directly in R

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
    
ourFunc() # After the sourceCpp, the function ourFunc worked get random 0.4178347 ~unif(1)

## Idefinite Summs ##
  #Taylor Expansions 

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
      }", file = "lab3_2.cpp")

sourceCpp("lab3_2.cpp")

logapprox(0.5, 10) # -0.6930649

log (0.5) #-0.6931472

#n=30
#a <- seq(0, 0, length.out = n)

for (k in 1:30){
  a<-logapprox(0.5, k)-log(0.5)
  print (a)
}

#k=19, 5.769999e-08

for (k in 1:30){
  a <-logapprox(0.01, k)-log(0.01)
  print (a)
}

for (k in 1:100){
  a <-logapprox(0.01, k)-log(0.01)
  print (a)
}
#No, the same value of k is not applicable here, when k=100 the decrease is still on going
#Try k=1000
for (k in 1:1000){
  a <-logapprox(0.01, k)-log(0.01)
  print (a)
}
#The best error can be achieved is 1.710219e-05, k=846

###########################################################################################
#The C++ code in lab3_2.cpp uses float. 
#float are single-point precision floating point values. 
#This means they use half the memory and are sometimes faster, but also less accurate. 
#Change logapprox to use double instead.
###########################################################################################
write("#include <Rcpp.h>
      using namespace Rcpp;
      // [[Rcpp::export]]
      double d_logapprox(double x, int k)
      {
       x = 1.0 - x;
       double sum = 0;	
          for (int i = 1; i <= k; i++)
          {
            double term = pow(x,i);
            sum -= 1.0 / i * term;
          }
        return sum;
      }", file = "lab3_3.cpp")

sourceCpp("lab3_3.cpp")

for (k in 1:100){
  a <- d_logapprox(0.5, k)-log(0.5)
  print (a)
}

# when k=48, the best error can be achieved is 2.220446e-16

for (k in 1:1000){
  a <- logapprox(0.01, k)-log(0.01)
  print (a)
}

for (k in 1:3000){
  a <- d_logapprox(0.01, k)-log(0.01)
  print (a)
}
#the best error can be achieved is 3.197442e-14
# double instead of float increases precision by 1e-9 while k increase from 1000 to 3000
#########################################################################################
### Kahan's summation Algorithm

write("#include <Rcpp.h>
       using namespace Rcpp;
       // [[Rcpp::export]]
       double ksa_logapprox(double x, int k)
       {
         x = 1.0 - x;
         double kc = 0;
         double ksum = 0;	
            for (int i = 1; i <= k; i++)
            {
              double kterm = -1.0 / i * pow(x, i) ;
              double  y = kterm - kc;
              kterm = ksum + y;
              kc = (kterm - ksum) - y;
              ksum = kterm;
            }
         return ksum;
        }", file="lab3_ksa.cpp")

sourceCpp("lab3_ksa.cpp")

ksa_logapprox(0.5, 100) #-0.6931472

#ksa_logapprox(0.01, 1e9) #-4.60517 (relatively slow to get the result)

for (k in 1:50){
  a <- ksa_logapprox(0.5, k)-log(0.5)
  print (a)
}
# best error is 1.110223e-16 at k=48 then to 0 when k>48

for (k in 1:4000){
  a <- ksa_logapprox(0.01, k)-log(0.01)
  print (a)
}
# best error is 8.881784e-16, comparing to lab3_3.cpp, the precision is 1e-16
# k increase to 4000
# singel precision (0.5) is better than double precision (0.01)

###################################################################################
#Timing results

#Function in R
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
    t = - 1.0 / (1:k) * x^(1:k)
    sum = sum(t)
    return(sum)
}


## Benchmark
library(microbenchmark)

microbenchmark(d_logapprox(0.01, k),
              logapproxR1(0.01, k),
              logapproxR2(0.01, k))

# Unit: microseconds
#                 expr    min     lq      mean  median      uq     max neval
# d_logapprox(0.01, k)  4.346  8.297   9.89771   9.088   9.878  64.799   100
# logapproxR1(0.01, k) 58.477 94.037 100.45732 102.334 110.631 199.137   100
# logapproxR2(0.01, k)  8.298 14.619  15.71768  15.805  16.792  28.448   100

## Q: How does the Rcpp version fare compared to an R loop and a vectorised R code?
   # Rcpp is much faster than the R function with for loop and the R function with Vectorized form
   # on average, the R function with Vectorized form is about twice slower than Rcpp 
   # while the R function with for loop is about 10 times slower tha Rcpp. 

library(compiler)

logapproxRcompiled <- cmpfun(logapproxR1)

microbenchmark(d_logapprox(0.01,k), logapproxR1(0.01, k), 
               logapproxR2(0.01, k), logapproxRcompiled(0.01,k))

# Unit: microseconds
#                 expr    min     lq     mean median      uq     max neval
# d_logapprox(0.01, k)  3.556  4.347  5.83194  5.136  5.5320  28.843   100
# logapproxR1(0.01, k) 55.316 58.477 67.47338 65.588 66.9715 194.395   100
# logapproxR2(0.01, k)  7.507  8.297  9.39194  8.692  9.0880  31.609   100
# logapproxRcompiled(0.01, k) 12.248 13.434 15.09335 13.829 14.6190  45.438   100

### logapproxRcompiled () is faster than logapproxR1(), on average about 4 times faster.
### Rcpp is still the fastest.
###########################################################################################
#Q: Do you see any way to remove the evaluation of a pow and/or a division from the C++ code? 
#   Time the new version with microbenchmark on the x = 0.01 case and assess the benefit.

write("#include <Rcpp.h>
      using namespace Rcpp;
      // [[Rcpp::export]]
      double np_logapprox(double x, int k)
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
      }",file="lab3_rmpower.cpp")

sourceCpp("lab3_rmpower.cpp") 

microbenchmark(d_logapprox(0.01,k), logapproxR1(0.01, k), 
               logapproxR2(0.01, k), logapproxRcompiled(0.01,k), np_logapprox(0.01, k))

# Unit: microseconds
# expr      min        lq      mean    median        uq      max neval
# d_logapprox(0.01, k)  353.625  374.9615  391.2244  383.8515  397.6805  743.601   100
# logapproxR1(0.01, k) 4228.491 4390.4860 4623.4721 4472.4720 4591.0060 6415.831   100
# logapproxR2(0.01, k)  420.794  441.5380  456.4416  455.7625  468.0105  548.811   100
# logapproxRcompiled(0.01, k) 1026.502 1045.6645 1110.9725 1080.0395 1128.2430 2009.541   100
# np_logapprox(0.01, k)  156.860  161.6010  170.6056  168.1205  179.3810  194.395   100

microbenchmark(logapprox(0.01,k),  logapproxR1(0.01, k), 
               logapproxR2(0.01, k), logapproxRcompiled(0.01,k), np_logapprox(0.01, k))

# Unit: microseconds
# expr      min        lq      mean    median        uq      max neval
# logapprox(0.01, k)  354.416  376.1465  396.0605  385.0370  405.1875  677.222   100
# logapproxR1(0.01, k) 4252.987 4379.8180 4648.6921 4490.0545 4700.4520 6475.889   100
# logapproxR2(0.01, k)  422.375  437.1915  469.2946  452.9965  474.9250  855.418   100
# logapproxRcompiled(0.01, k) 1024.131 1047.4420 1106.1403 1081.4220 1102.9555 1977.537   100
# np_logapprox(0.01, k)  156.860  160.0200  173.8059  169.8980  179.1835  311.744   100

## The function after removing power turned out to be the fastest one.
##########################################################################################











