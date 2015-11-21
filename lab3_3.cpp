#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox2(double x, int k)
{
  x = 1.0 - x;
  
  double sum = 0;	
  for (int i = 1; i <= k; i++)
  {
    double term = pow(x,i);
    sum -= 1.0 / i * term;
  }
  
  return sum;
}
