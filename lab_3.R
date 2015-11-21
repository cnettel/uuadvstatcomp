#####LAB BLOCK 3######


###Install Rcpp.

install.packages("Rcpp")
library(Rcpp)


###We create our first C++ function in R.
###We have two options to load C++ code using Rcpp: cppFunction and sourceCpp
###We start with a function with cppFunction

Rfunc <- cppFunction('double ourFunc()
{
    return runif(1)[0];
}
');


###Try running this function. What do you write? 

Rfunc()


###What results do you get?

#we get a random number between 0 and 1 (0.9181206)


###We can inspect a function by writing its name and then see its "contents".

Rfunc
#function () 
#.Primitive(".Call")(<pointer: 0x6f081a30>)


###In R, We can load a C++ source file using 'sourceCpp'
###We save our code as the file "lab3_1.cpp", and source it.

sourceCpp("lab3_1.cpp")


###The name of the C++ function becomes the name of the corresponding R function.
###Go ahead, try using ourFunc. Does it work?

ourFunc()

#Yes (0.4178406)

##############################################################################

###Indefinite sums###


###we will use the Taylor expansion for log(1-x) to approximate the log of a number
###We create this function in "lab3_2.cpp" and source it.

sourceCpp("lab3_2.cpp")

###This function accepts a value x and a positive number of expansion terms k.

logapprox(0.5, 10)  # -0.6930649


###Is the value close to the log of 0.5?

#yes

log(0.5)   # -0.6931472


###How many terms do you need to make it "close?

logapprox(0.5, 16)-log(0.5)  # 7.729557e-07
logapprox(0.5, 17)-log(0.5)  # 2.961186e-07
logapprox(0.5, 18)-log(0.5)  # 5.769999e-08
logapprox(0.5, 19)-log(0.5)  # -6.15093e-08
logapprox(0.5, 20)-log(0.5)  # -1.211139e-07
logapprox(0.5, 21)-log(0.5)  # -1.211139e-07

# k=20


###Now, we want to approximate the logarithm of 0.01.

log(0.01)  # -4.60517


logapprox(0.01, 19)-log(0.01)  # 1.239194
logapprox(0.01, 219)-log(0.01) # 0.03688949
logapprox(0.01, 700)-log(0.01) # 0.0001119928
logapprox(0.01, 843)-log(0.01) # 1.85327e-05
logapprox(0.01, 844)-log(0.01) # 1.805586e-05
logapprox(0.01, 845)-log(0.01) # 1.757902e-05
logapprox(0.01, 846)-log(0.01) # 1.710219e-05
logapprox(0.01, 847)-log(0.01) # 1.710219e-05

###Is the same value of k applicable? 

#No, k= 846. 

###How big is the best error you can achieve?

#1.710219e-05


###Change logapprox to use double instead of float.
###We create this function in "lab3_3.cpp" and source it.

sourceCpp("lab3_3.cpp")


###Does the error decrease for 0.5 and 0.01 with the same k as before?

logapprox2(0.5, 20)-log(0.5)  # 4.350892e-08
logapprox2(0.01, 846)-log(0.01) # 2.143963e-05

#No, the error increases.


###What happens now if you increase k?

logapprox2(0.5, 46)-log(0.5)  # 4.440892e-16
logapprox2(0.5, 47)-log(0.5)  # 3.330669e-16
logapprox2(0.5, 48)-log(0.5)  #2.220446e-16
logapprox2(0.5, 49)-log(0.5)  #2.220446e-16

#The error decreases. (k=48)


logapprox2(0.01, 2000)-log(0.01) # 8.804513e-11
logapprox2(0.01, 2700)-log(0.01) #5.77316e-14
logapprox2(0.01, 2728)-log(0.01) #3.28626e-14
logapprox2(0.01, 2729)-log(0.01) #3.197442e-14
logapprox2(0.01, 2730)-log(0.01) #3.108624e-14
logapprox2(0.01, 2731)-log(0.01) #3.108624e-14

#The error decreases. (k=2730)


###Can you see any possible improvements to this scheme?

#we could sum over all columns.


###We try implementing Kahan's summation algorithm.
###We create this function in "lab3_4.cpp" and source it.


sourceCpp("lab3_4.cpp")  #single
sourceCpp("lab3_5.cpp")  #double

###Can we reduce the error for single precision for x = 0.01 using Kahan's algorithm?

logapprox3(0.01, 847)-log(0.01) # 1.686498e-05

#yes(very little)


###Can we reduce the error for double precision for x = 0.01 using Kahan's algorithm?
logapprox4(0.01, 2730)-log(0.01) #3.108624e-14

#no


#################################################################################


###Timing results###


###write a version of logapprox in R


logapproxR <-function(x, k)
{
  x <- 1.0 - x;
  sum <- 0;  
  for(i in 1:k)
  {
    term <- x^i;
    sum <- sum-( 1.0 / i * term);
  }
  sum  
}


###Run a command like microbenchmark

install.packages("microbenchmark")
library(microbenchmark)


k=2000
microbenchmark(logapprox2(0.01,k), logapproxR(0.01, k))
#Unit: microseconds
#expr      min       lq     mean    median        uq      max neval
#logapprox2(0.01, k)  564.008  600.151  656.659  682.4765  701.8865  726.874   100
#logapproxR(0.01, k) 5704.781 5843.105 6637.687 6846.1830 6905.7515 8936.672   100


###How does the Rcpp version fare compared to an R loop and a vectorised R code?

#Rcpp is 10 times faster than an R loop



###We use cmpfun from the R compiler package to do a byte code compilation of the R loop.

library(compiler)  

logapproxRcompiled <- cmpfun(logapproxR)

microbenchmark(logapprox2(0.01,k), logapproxRcompiled(0.01, k))

#Unit: microseconds
#expr      min        lq      mean    median       uq      max neval
#logapprox2(0.01, k)  673.329  686.2695  702.1902  701.2175  709.472  982.999   100
#logapproxRcompiled(0.01, k) 1260.094 1373.8770 1476.6123 1489.6685 1540.313 3294.807   100



###Is this version "fast"?

#Now Rcpp is only 2 times faster than an R loop



###Do you see any way to remove the evaluation of a pow or a division from the C++ code?

###We create a new function in "lab3_6.cpp" and source it.

sourceCpp("lab3_6.cpp")


###Time your new version with microbenchmark on the x = 0.01 case and assess the benefit.

microbenchmark(logapproxC(0.01,k), logapproxRcompiled(0.01, k))

#Rcpp is 10 times faster than before.




