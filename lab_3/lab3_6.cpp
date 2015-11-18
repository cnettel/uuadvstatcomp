#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapproxf(double x, int k)
{
  x = 1.0 - x;
  
  float sum  = 0;
  float term = 1;
  for (int i = 1; i <= k; i++)
  {
  term = term*x;
  sum -= 1.0 / i * term;
  }
  
  return sum;
}
