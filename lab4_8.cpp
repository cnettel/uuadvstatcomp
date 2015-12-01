#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox7(double x, int k) 
{
  x = 1.0 - x;
  
  double sum = 0;
  double newpow = x;
  for (int i = 1; i <= k; i++)
  {
      double term = -1.0 / i * newpow;
      sum += term;
      newpow = newpow*x;
  }
  
  return sum;
}